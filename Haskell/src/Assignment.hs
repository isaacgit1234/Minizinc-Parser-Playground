module Assignment
  ( ADT (..),
    Declaration (..),
    Domain (..),
    Expr (..),
    Constraint (..),
    CompareOperator (..),
    SolverGoal (..),
    miniZincParser,
    solveProblem,
    getTime,
    formatSolution,
  )
where

import Control.Applicative (many, optional, (<|>))
import Control.Monad (foldM, guard, unless)
import Data.List (delete, minimumBy, nub)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe, maybeToList)
import Data.Ord (comparing)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Instances (ParseError (UnexpectedString), Parser)
import Parser
  ( alpha,
    charTok,
    commaTok,
    digit,
    eof,
    failed,
    int,
    is,
    sepBy1,
    spaces,
    stringTok,
    tok,
  )

--------------------------------------------------------------------------------
-- AST
--------------------------------------------------------------------------------

-- | An identifier is just a name: a variable, constant, or array name
-- as it appears in the MiniZinc-Lite source.
type Identifier = String

-- | The full parsed program: its declarations, its constraints, and
-- what the solver should do (satisfy, maximize, or minimize).
data ADT = ADT
  { declarations :: [Declaration],
    constraints :: [Constraint],
    solverGoal :: SolverGoal
  }
  deriving (Eq, Show)

-- | A single top-level declaration: a named constant bound to an
-- expression, a named variable with a domain to search over, or an
-- array of variables sharing one domain.
data Declaration
  = ConstantDeclaration Identifier Expr
  | VariableDeclaration Identifier Domain
  | ArrayDeclaration Identifier Expr Domain
  deriving (Eq, Show)

-- | The set of values a variable can take: either an inclusive
-- integer range (bounds given as expressions, so they can reference
-- constants) or an explicit finite set of integers.
data Domain
  = Range Expr Expr
  | FiniteSet [Int]
  deriving (Eq, Show)

-- | An arithmetic expression: literals, variable/constant references,
-- negation, binary operators, and array indexing.
data Expr
  = IntLit Int
  | Var Identifier
  | Neg Expr
  | BinOp Operator Expr Expr
  | Index Identifier Expr
  deriving (Eq, Show)

-- | The arithmetic operators usable inside an expression.
data Operator = Add | Sub | Mul | Div | Pow
  deriving (Eq, Show)

-- | The six relational operators usable in a constraint comparison.
data CompareOperator = OpEq | OpNeq | OpLt | OpLe | OpGt | OpGe
  deriving (Eq, Show)

-- | The two argument forms every macro accepts, for any element type
-- 'a' (an 'Expr' for sum/min/max/allDifferent, a 'Constraint' for
-- all/any): a literal list, or a generator that builds one element
-- per value in a domain.
data Args a
  = ListArgs [a]
  | GeneratorArgs Identifier Domain a
  deriving (Eq, Show)

-- | The arguments of sum/min/max/allDifferent: the shared forms over
-- expressions, or a whole array by name. Only these macros get the
-- array form, because array elements are numbers, not comparisons.
data MacroArgs
  = ExprArgs (Args Expr)
  | ArrayForm Identifier
  deriving (Eq, Show)

-- | A single constraint: a direct comparison, a numeric aggregate
-- macro (sum/min/max) compared against a value, an all/any grouping
-- of sub-constraints, or an allDifferent macro.
data Constraint
  = Compare CompareOperator Expr Expr
  | NumericMacro NumericMacroName MacroArgs CompareOperator Expr
  | AllMacro (Args Constraint)
  | AnyMacro (Args Constraint)
  | AllDiffMacro MacroArgs
  deriving (Eq, Show)

-- | Which aggregate a NumericMacro applies to its argument list.
data NumericMacroName = SumMacro | MinMacro | MaxMacro
  deriving (Eq, Show)

-- | What the solver is asked to do: find any satisfying assignment,
-- or find the one that maximizes/minimizes a given expression.
data SolverGoal
  = Satisfy
  | Maximize Expr
  | Minimize Expr
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

-- | Parses an identifier: a letter followed by letters, digits, or
-- underscores, consuming any trailing whitespace.
identifier :: Parser Identifier
identifier = tok ((:) <$> alpha <*> many (alpha <|> digit <|> is '_'))

-- | Parses the smallest units of an expression: integer literals,
-- bare or indexed identifiers, and parenthesized sub-expressions.
atomParser :: Parser Expr
atomParser =
  (IntLit <$> tok int)
    <|> (varOrIndex <$> identifier <*> optional index)
    <|> (charTok '(' *> exprParser <* charTok ')')
  where
    index = charTok '[' *> exprParser <* charTok ']'
    -- No "[...]" after the name: a plain variable. Otherwise: indexing.
    varOrIndex name = maybe (Var name) (Index name)

-- | Parses exponentiation, right-associative and binding tighter
-- than multiplication/division.
powerParser :: Parser Expr
powerParser = powerOf <$> atomParser <*> optional (charTok '^' *> unaryParser)
  where
    -- No "^ exponent" after the base: just the base itself.
    powerOf base = maybe base (BinOp Pow base)

-- | Parses unary minus. It binds looser than '^' but tighter than
-- '*' and '/', so "-x^2" is "-(x^2)" while "-x * y" is "(-x) * y".
-- The '-' branch is tried first so 'int' never sees a leading '-'.
unaryParser :: Parser Expr
unaryParser = (Neg <$> (charTok '-' *> unaryParser)) <|> powerParser

-- | Parses operands separated by left-associative operators,
-- e.g. "a - b - c" becomes "(a - b) - c".
chainLeft :: Parser Expr -> Parser Operator -> Parser Expr
chainLeft operand operator =
  foldl (\acc (op, e) -> BinOp op acc e)
    <$> operand
    <*> many ((,) <$> operator <*> operand)

-- | Parses a chain of multiplication/division, left-associative,
-- binding tighter than addition/subtraction.
termParser :: Parser Expr
termParser = chainLeft unaryParser mulOp
  where
    mulOp = (Mul <$ charTok '*') <|> (Div <$ charTok '/')

-- | Parses a full expression: a chain of addition/subtraction over
-- terms, left-associative. The entry point for expression parsing.
exprParser :: Parser Expr
exprParser = chainLeft termParser addOp
  where
    addOp = (Add <$ charTok '+') <|> (Sub <$ charTok '-')

-- | Parses a domain: either an inclusive range (lo..hi) or an
-- explicit finite set {v1, v2, ...} with no duplicate values.
domainParser :: Parser Domain
domainParser = rangeParser <|> finiteSetParser
  where
    rangeParser = Range <$> exprParser <*> (stringTok ".." *> exprParser)
    finiteSetParser = do
      xs <- charTok '{' *> (tok int `sepBy1` commaTok) <* charTok '}'
      failUnless (allUnique xs) "Duplicate values in finite set"
      pure (FiniteSet xs)

-- | Parses one top-level declaration: a constant (int: name = expr;),
-- an array (array[1..n] of var dom: name;), or a plain variable
-- (var dom: name;). Array lower bounds must literally be 1.
declarationParser :: Parser Declaration
declarationParser = constantDecl <|> arrayDecl <|> variableDecl
  where
    constantDecl =
      ConstantDeclaration
        <$> (stringTok "int" *> charTok ':' *> identifier)
        <*> (charTok '=' *> exprParser <* charTok ';')

    arrayDecl = do
      lo <- stringTok "array" *> charTok '[' *> tok int <* stringTok ".."
      failUnless (lo == 1) "array lower bound must be 1"
      lenExpr <-
        exprParser <* charTok ']' <* stringTok "of" <* stringTok "var"
      dom <- domainParser <* charTok ':'
      name <- identifier <* charTok ';'
      pure (ArrayDeclaration name lenExpr dom)

    variableDecl =
      flip VariableDeclaration
        <$> (stringTok "var" *> domainParser <* charTok ':')
        <*> (identifier <* charTok ';')

-- | Post-parse validation of a declaration list: rejects duplicate
-- identifiers, domain bounds that reference an undeclared or
-- later-declared constant, and ranges where the lower bound exceeds
-- the upper bound. Returns the fully evaluated constant bindings on
-- success, reusing evalExpr against a throwaway ConstraintProblem
-- rather than duplicating evaluation logic.
validateDeclaration :: [Declaration] -> Maybe [(Identifier, Int)]
validateDeclaration decls =
  guard (allUnique (map declName decls)) *> foldM checkDecl [] decls
  where
    eval consts = evalExpr (emptyProblem {searchConstants = consts})

    -- One fold step: check a declaration against the constants seen
    -- so far, and return the (possibly extended) constants.
    checkDecl consts (ConstantDeclaration name e) =
      (\v -> (name, v) : consts) <$> eval consts e
    checkDecl consts (VariableDeclaration _ dom) =
      consts <$ checkDomain consts dom
    checkDecl consts (ArrayDeclaration _ lenExpr dom) = do
      len <- eval consts lenExpr
      checkDomain consts dom
      guard (len >= 1)
      pure consts

    checkDomain consts (Range lo hi) = do
      loV <- eval consts lo
      hiV <- eval consts hi
      guard (loV <= hiV)
    checkDomain _ (FiniteSet _) = Just ()

-- | Parses one "constraint ...;" statement: a numeric macro, an
-- allDifferent macro, an all/any grouping, or a plain comparison,
-- tried in that order so macros aren't mistaken for comparisons.
constraintParser :: Parser Constraint
constraintParser =
  stringTok "constraint"
    *> (numericMacro <|> allDifferent <|> allAny <|> compareExpr)
    <* charTok ';'
  where
    numericMacro =
      NumericMacro
        <$> numericMacroName
        <*> macroArgsParser
        <*> relOp
        <*> exprParser

    allDifferent =
      AllDiffMacro <$> (stringTok "allDifferent" *> macroArgsParser)

    allAny =
      (AllMacro <$> (stringTok "all" *> argsParser compareExpr))
        <|> (AnyMacro <$> (stringTok "any" *> argsParser compareExpr))

    numericMacroName =
      (SumMacro <$ stringTok "sum")
        <|> (MinMacro <$ stringTok "min")
        <|> (MaxMacro <$ stringTok "max")

    -- sum/min/max/allDifferent arguments: the shared forms over
    -- expressions, or a bare array name "(q)".
    macroArgsParser :: Parser MacroArgs
    macroArgsParser =
      (ExprArgs <$> argsParser exprParser)
        <|> (ArrayForm <$> (charTok '(' *> identifier <* charTok ')'))

    -- The forms every macro accepts, for any element parser: a
    -- generator "(i in dom)(item)" or a literal list "([item, ...])".
    argsParser :: Parser a -> Parser (Args a)
    argsParser item = generator item <|> (ListArgs <$> bracketList item)

    -- "(i in dom)(body)": one body per value of i in dom.
    generator body =
      GeneratorArgs
        <$> (charTok '(' *> identifier)
        <*> (stringTok "in" *> domainParser <* charTok ')')
        <*> (charTok '(' *> body <* charTok ')')

    -- "([x, y, ...])": a non-empty list in brackets and parentheses.
    bracketList item =
      charTok '('
        *> charTok '['
        *> (item `sepBy1` commaTok)
        <* charTok ']'
        <* charTok ')'

    -- "<expr> <op> <expr>": a plain comparison.
    compareExpr = flip Compare <$> exprParser <*> relOp <*> exprParser

    relOp =
      (OpEq <$ stringTok "=")
        <|> (OpNeq <$ stringTok "!=")
        <|> (OpLe <$ stringTok "<=")
        <|> (OpGe <$ stringTok ">=")
        <|> (OpLt <$ stringTok "<")
        <|> (OpGt <$ stringTok ">")

-- | All identifiers referenced within an expression, including the
-- array name of an Index node.
exprNames :: Expr -> [Identifier]
exprNames (IntLit _) = []
exprNames (Var x) = [x]
exprNames (Neg e) = exprNames e
exprNames (BinOp _ l r) = exprNames l ++ exprNames r
exprNames (Index name idxExpr) = name : exprNames idxExpr

-- | All identifiers referenced by a domain's bounds.
domainNames :: Domain -> [Identifier]
domainNames (Range lo hi) = exprNames lo ++ exprNames hi
domainNames (FiniteSet _) = []

-- | All identifiers referenced within a macro's arguments, given how
-- to collect the names in one element. A generator's own bound
-- variable is excluded from its body's references (it is locally
-- bound, not a declaration).
argsNames :: (a -> [Identifier]) -> Args a -> [Identifier]
argsNames names (ListArgs xs) = concatMap names xs
argsNames names (GeneratorArgs var dom body) =
  domainNames dom ++ filter (/= var) (names body)

-- | All identifiers referenced within a sum/min/max/allDifferent
-- macro's arguments.
macroArgsNames :: MacroArgs -> [Identifier]
macroArgsNames (ExprArgs args) = argsNames exprNames args
macroArgsNames (ArrayForm name) = [name]

-- | All identifiers referenced within a constraint, recursing through
-- all/any groupings.
constraintNames :: Constraint -> [Identifier]
constraintNames (Compare _ a b) = exprNames a ++ exprNames b
constraintNames (NumericMacro _ margs _ e) =
  macroArgsNames margs ++ exprNames e
constraintNames (AllMacro args) = argsNames constraintNames args
constraintNames (AnyMacro args) = argsNames constraintNames args
constraintNames (AllDiffMacro margs) = macroArgsNames margs

-- | All identifiers referenced by a solve goal's objective.
goalNames :: SolverGoal -> [Identifier]
goalNames Satisfy = []
goalNames (Maximize e) = exprNames e
goalNames (Minimize e) = exprNames e

-- | Parses the trailing "solve satisfy/maximize/minimize ...;"
-- directive.
solveParser :: Parser SolverGoal
solveParser = stringTok "solve" *> goal <* charTok ';'
  where
    goal = satisfyGoal <|> maximizeGoal <|> minimizeGoal
    satisfyGoal = Satisfy <$ stringTok "satisfy"
    maximizeGoal = Maximize <$> (stringTok "maximize" *> exprParser)
    minimizeGoal = Minimize <$> (stringTok "minimize" *> exprParser)

-- | One statement before the solve directive, so declarations and
-- constraints can appear in any order.
data Statement = DeclStmt Declaration | ConsStmt Constraint

-- | True if every constraint only mentions names declared above it.
declaredBeforeUse :: [Statement] -> Bool
declaredBeforeUse = isJust . foldM visit []
  where
    -- The accumulator is every name declared so far.
    visit seen (DeclStmt d) = Just (declName d : seen)
    visit seen (ConsStmt c) =
      seen <$ guard (all (`elem` seen) (constraintNames c))

-- | The name a declaration introduces.
declName :: Declaration -> Identifier
declName (ConstantDeclaration name _) = name
declName (VariableDeclaration name _) = name
declName (ArrayDeclaration name _ _) = name

-- | Succeeds with () when the check holds, otherwise fails the parse
-- with the given message. Used after a value has been parsed, when
-- whether to accept it depends on that value.
failUnless :: Bool -> String -> Parser ()
failUnless ok message = unless ok (failed (UnexpectedString message))

-- | True if no value appears more than once.
allUnique :: (Eq a) => [a] -> Bool
allUnique xs = length xs == length (nub xs)

-- | The top-level parser for a whole MiniZinc-Lite program: any mix of
-- declarations and constraints, then exactly one solve directive, then
-- end of input. Declarations are validated after parsing, each
-- constraint may only use names declared above it, and the objective
-- may only use declared names.
miniZincParser :: Parser ADT
miniZincParser = do
  stmts <- spaces *> many statementParser
  let decls = [d | DeclStmt d <- stmts]
      cons = [c | ConsStmt c <- stmts]
  failUnless (isJust (validateDeclaration decls)) "invalid declarations"
  failUnless (declaredBeforeUse stmts) undeclared
  goal <- solveParser <* eof
  failUnless (all (`elem` map declName decls) (goalNames goal)) undeclared
  pure (ADT decls cons goal)
  where
    statementParser =
      (DeclStmt <$> declarationParser) <|> (ConsStmt <$> constraintParser)
    undeclared = "use of an undeclared identifier"

--------------------------------------------------------------------------------
-- Solver
--------------------------------------------------------------------------------

-- | A resolved name-to-value pair: either a solved constant or a
-- solved variable.
type Binding = (Identifier, Int)

-- | The solver's working state as it searches: which constants and
-- variables have been solved so far, what's left to solve, the
-- constraints to check against, and the lengths of any declared
-- arrays (needed to evaluate Index expressions and ArrayForm macros).
data ConstraintProblem = ConstraintProblem
  { searchConstants :: [Binding],
    solvedVars :: [Binding],
    remainingDeclarations :: [Declaration],
    searchConstraints :: [Constraint],
    arrayLengths :: [(Identifier, Int)]
  }

-- | A problem with nothing declared, solved, or constrained yet. The
-- solver and the declaration checks start from it and fill in only
-- the fields they need.
emptyProblem :: ConstraintProblem
emptyProblem =
  ConstraintProblem
    { searchConstants = [],
      solvedVars = [],
      remainingDeclarations = [],
      searchConstraints = [],
      arrayLengths = []
    }

-- | Solves a fully-parsed program according to its solve goal:
-- the first satisfying assignment for "satisfy", or the
-- maximizing/minimizing assignment (by the given expression) for
-- "maximize"/"minimize". Returns Nothing if no solution exists.
solveProblem :: ADT -> Maybe [Binding]
solveProblem adt =
  toBindings <$> case solverGoal adt of
    Satisfy -> listToMaybe (search problem)
    Maximize e -> bestSolution OpGt e problem
    Minimize e -> bestSolution OpLt e problem
  where
    problem =
      emptyProblem
        { remainingDeclarations = declarations adt,
          searchConstraints = constraints adt
        }

    toBindings :: ConstraintProblem -> [Binding]
    toBindings p = reverse (solvedVars p)

-- | Branch and bound (Part E): walks the search tree once, carrying the
-- best (value, solution) found so far. Every node must also satisfy
-- "objective `better` best value", so forward checking cuts any branch
-- that cannot improve, and no part of the tree is searched twice.
bestSolution ::
  CompareOperator -> Expr -> ConstraintProblem -> Maybe ConstraintProblem
bestSolution better e start = snd <$> go Nothing start
  where
    go best p = case children q of
      Nothing -> leaf
      Just kids -> foldl go best kids
      where
        q = withBound best p
        leaf = case evalExpr q e of
          Just v | allSatisfied q -> Just (v, q)
          _ -> best

    -- The original constraints plus "must beat the best value so far".
    withBound Nothing p = p
    withBound (Just (v, _)) p =
      p {searchConstraints = beats v : searchConstraints start}
    beats v = Compare better e (IntLit v)

-- | Backtracking search over the remaining declarations. It returns
-- every complete, satisfying assignment lazily, so 'listToMaybe' stops
-- after the first one for "solve satisfy".
--
-- Part B (incremental checking): a variable only tries values from
-- 'legalValues', which drops any value that makes an already
-- evaluable constraint false, so bad branches stop immediately.
--
-- Part E (extension: faster solving):
--   * Branch and bound ('bestSolution'): maximize/minimize walk the
--     search tree once, carrying the best value so far; every node
--     must beat it, so forward checking cuts branches that can't.
-- Measured against plain backtracking (median of 3 runs, -O1):
--   N-Queens 14, satisfy:     0.23s -> 0.04s (about 6x faster)
--   SEND+MORE, satisfy:       6.3s  -> 4.7s
--   N-Queens 10, minimize:    1.4s  -> 0.02s
--   N-Queens 12, minimize:    ~52s  -> 0.04s
--   maximize x + y + z, 1..50: 0.03s -> 0.02s (no real gain: the
--     bound is only known at the leaves, so there is little to cut)
--
--   * Forward checking ('forwardCheck'): after each assignment, every
--     remaining variable's domain is shrunk to its still-legal values
--     and stored, so a branch where some variable has no values left
--     is cut at once, and deeper levels never redo that work.
--   * MRV ordering ('mostConstrainedFirst'): branch next on the
--     variable with the fewest values left, so failures show up early.
--     (Both from Russell & Norvig, AIMA, Ch. 6.)
search :: ConstraintProblem -> [ConstraintProblem]
search p = case children p of
  Nothing -> [p | allSatisfied p]
  Just kids -> concatMap search kids

-- | One level of the search tree: Nothing at a leaf (everything is
-- solved), otherwise the states after solving the next declaration,
-- which MRV picks.
children :: ConstraintProblem -> Maybe [ConstraintProblem]
children p = case mostConstrainedFirst p (remainingDeclarations p) of
  [] -> Nothing
  d : rest -> Just (solveNext d p {remainingDeclarations = rest})

-- | Solves one declaration (already taken off the to-do list) and
-- returns every state it can lead to: the node's children in the
-- search tree. Written in the list monad, where a list is "every
-- possible outcome": '<-' tries each option in turn, 'guard' drops a
-- branch, and 'maybeToList' turns a failed evaluation (Nothing) into
-- no options ([]).
solveNext :: Declaration -> ConstraintProblem -> [ConstraintProblem]
solveNext (ConstantDeclaration name e) p = do
  v <- maybeToList (evalExpr p e)
  let p' = p {searchConstants = (name, v) : searchConstants p}
  guard (not (violatesConstraints p'))
  pure p'
solveNext (VariableDeclaration name dom) p = do
  v <- legalValues p name dom
  maybeToList (forwardCheck (assign name v p))
solveNext (ArrayDeclaration name lenExpr dom) p = do
  len <- maybeToList (evalExpr p lenExpr)
  guard (len >= 1)
  pure
    p
      { remainingDeclarations = elemDecls len ++ remainingDeclarations p,
        arrayLengths = (name, len) : arrayLengths p
      }
  where
    elemDecls len =
      [VariableDeclaration (arrayElemName name i) dom | i <- [1 .. len]]

-- | Records the assignment name := v as solved.
assign :: Identifier -> Int -> ConstraintProblem -> ConstraintProblem
assign name v p = p {solvedVars = (name, v) : solvedVars p}

-- | Builds the internal name used for the i-th element of an array,
-- e.g. "q" and 3 become "q[3]".
arrayElemName :: Identifier -> Int -> Identifier
arrayElemName name i = name ++ "[" ++ show i ++ "]"

-- | True if any constraint is definitely broken under the current
-- (possibly partial) assignment.
violatesConstraints :: ConstraintProblem -> Bool
violatesConstraints p = any broken (searchConstraints p)
  where
    broken c = evalConstraint p c == Just False

-- | The values from dom that remain consistent with already-solved
-- variables: assigning name := v right now wouldn't break any
-- constraint whose other variables are already known.
legalValues :: ConstraintProblem -> Identifier -> Domain -> [Int]
legalValues p name dom = filter allowed (domainValues p dom)
  where
    allowed v = not (violatesConstraints (assign name v p))

-- | Forward checking: after an assignment, shrinks every remaining
-- variable's domain to the values still consistent with it, and
-- stores the shrunk domain so deeper levels never redo the work.
-- Nothing means some variable has no values left: a dead branch.
forwardCheck :: ConstraintProblem -> Maybe ConstraintProblem
forwardCheck p =
  (\decls -> p {remainingDeclarations = decls})
    <$> traverse prune (remainingDeclarations p)
  where
    -- A domain that uses a constant not solved yet can't be pruned
    -- yet: the guard fails and the last equation keeps it as it is.
    prune (VariableDeclaration name dom)
      | isJust (domainValuesMaybe p dom) =
          case legalValues p name dom of
            [] -> Nothing
            vs -> Just (VariableDeclaration name (FiniteSet vs))
    prune d = Just d

-- | MRV: moves the variable with the fewest values left to the front.
-- Domains are already pruned, so this only compares sizes.
mostConstrainedFirst :: ConstraintProblem -> [Declaration] -> [Declaration]
mostConstrainedFirst p decls = case span isVariable decls of
  ([], _) -> decls
  (vars, rest) ->
    let best = minimumBy (comparing domainSize) vars
     in best : delete best vars ++ rest
  where
    isVariable (VariableDeclaration _ _) = True
    isVariable _ = False
    domainSize (VariableDeclaration _ dom) = length (domainValues p dom)
    domainSize _ = 0

-- | All values in a domain, evaluated against current constants:
-- a Range expands to the inclusive list between its bounds (or
-- empty if a bound can't yet be evaluated); a FiniteSet is used
-- as-is.
domainValues :: ConstraintProblem -> Domain -> [Int]
domainValues p = fromMaybe [] . domainValuesMaybe p

-- | Like domainValues, but Nothing (rather than []) if a Range
-- bound can't yet be evaluated -- used where a missing bound should
-- fail the whole computation rather than silently produce no values.
domainValuesMaybe :: ConstraintProblem -> Domain -> Maybe [Int]
domainValuesMaybe p (Range lo hi) =
  enumFromTo <$> evalExpr p lo <*> evalExpr p hi
domainValuesMaybe _ (FiniteSet xs) = Just xs

-- | Replaces every free occurrence of a variable with a given value
-- throughout an expression -- used to instantiate a generator
-- macro's body for each value the generator variable ranges over.
substitute :: Identifier -> Expr -> Expr -> Expr
substitute var val = go
  where
    go (IntLit n) = IntLit n
    go (Var x)
      | x == var = val
      | otherwise = Var x
    go (Neg e) = Neg (go e)
    go (BinOp op l r) = BinOp op (go l) (go r)
    go (Index name idxExpr) = Index name (go idxExpr)

-- | Expands a macro's arguments into the concrete elements they
-- denote: a literal list as-is, or a generator's body once per domain
-- value, using the given substitution to plug each value in for the
-- bound variable ('substitute' for expressions, 'substituteConstraint'
-- for comparisons).
resolveArgs ::
  (Identifier -> Expr -> a -> a) -> ConstraintProblem -> Args a -> Maybe [a]
resolveArgs _ _ (ListArgs xs) = Just xs
resolveArgs subst p (GeneratorArgs var dom body) =
  map (\v -> subst var (IntLit v) body) <$> domainValuesMaybe p dom

-- | Expands sum/min/max/allDifferent arguments into expressions: the
-- shared forms via 'resolveArgs', or an array name into one Index
-- expression per element, using the array's known length.
resolveMacroArgs :: ConstraintProblem -> MacroArgs -> Maybe [Expr]
resolveMacroArgs p (ExprArgs args) = resolveArgs substitute p args
resolveMacroArgs p (ArrayForm name) =
  (\len -> [Index name (IntLit i) | i <- [1 .. len]])
    <$> lookup name (arrayLengths p)

-- | Like 'substitute', but replaces every free occurrence of a
-- variable throughout a whole constraint (recursing into nested
-- macros) rather than a single expression. A nested generator that
-- rebinds the same name shadows the outer substitution, matching
-- ordinary variable-scoping rules.
substituteConstraint :: Identifier -> Expr -> Constraint -> Constraint
substituteConstraint var val = go
  where
    sub = substitute var val

    go (Compare op a b) = Compare op (sub a) (sub b)
    go (NumericMacro name margs op e) =
      NumericMacro name (subMacroArgs margs) op (sub e)
    go (AllMacro args) = AllMacro (subArgs go args)
    go (AnyMacro args) = AnyMacro (subArgs go args)
    go (AllDiffMacro margs) = AllDiffMacro (subMacroArgs margs)

    subMacroArgs (ExprArgs args) = ExprArgs (subArgs sub args)
    subMacroArgs (ArrayForm name) = ArrayForm name

    -- Applies f to every element, except inside a generator that
    -- rebinds the same name (its body means the inner variable).
    subArgs f (ListArgs xs) = ListArgs (map f xs)
    subArgs f g@(GeneratorArgs v d b)
      | v == var = g
      | otherwise = GeneratorArgs v d (f b)

-- | Evaluates an expression to a concrete integer against the
-- current (possibly partial) assignment, failing with Nothing if
-- it references a value not yet known or divides by zero.
evalExpr :: ConstraintProblem -> Expr -> Maybe Int
evalExpr _ (IntLit n) = Just n
evalExpr p (Var x) = lookup x (searchConstants p) <|> lookup x (solvedVars p)
evalExpr p (Neg e) = negate <$> evalExpr p e
evalExpr p (BinOp op l r) = do
  lv <- evalExpr p l
  rv <- evalExpr p r
  applyOperator op lv rv
evalExpr p (Index name idxExpr) = do
  idx <- evalExpr p idxExpr
  len <- lookup name (arrayLengths p)
  guard (1 <= idx && idx <= len)
  evalExpr p (Var (arrayElemName name idx))

-- | Evaluates a constraint to Just True/False once it can be fully
-- resolved against the current assignment, or Nothing while it still
-- depends on an unassigned variable. AllDiffMacro and 'all' can answer
-- False early, as soon as one argument is known to break them, even
-- with others still unknown.
evalConstraint :: ConstraintProblem -> Constraint -> Maybe Bool
evalConstraint p (Compare op a b) =
  applyCompare op <$> evalExpr p a <*> evalExpr p b
evalConstraint p (NumericMacro name margs op r) = do
  es <- resolveMacroArgs p margs
  vs <- mapM (evalExpr p) es
  rv <- evalExpr p r
  total <- aggregate name vs
  pure (applyCompare op total rv)
  where
    aggregate SumMacro = Just . sum
    aggregate MinMacro = fmap minimum . nonEmpty
    aggregate MaxMacro = fmap maximum . nonEmpty
evalConstraint p (AllDiffMacro margs) =
  resolveMacroArgs p margs >>= distinct
  where
    distinct es
      | not (allUnique known) = Just False
      | length known == length es = Just True
      | otherwise = Nothing
      where
        known = mapMaybe (evalExpr p) es
evalConstraint p (AllMacro args) = do
  cs <- resolveArgs substituteConstraint p args
  combineAll (map (evalConstraint p) cs)
evalConstraint p (AnyMacro args) = do
  cs <- resolveArgs substituteConstraint p args
  combineAny (map (evalConstraint p) cs)

-- | Combines Maybe Bool results for an "all" grouping: definitely
-- False if any is False, definitely True only once every one is
-- known True, otherwise still undecided.
combineAll :: [Maybe Bool] -> Maybe Bool
combineAll rs
  | Just False `elem` rs = Just False
  | all (== Just True) rs = Just True
  | otherwise = Nothing

-- | Combines Maybe Bool results for an "any" grouping: undecided while
-- any one is unknown, otherwise True if any is True, else False. Once
-- every variable is assigned, an unknown can only be a division by zero
-- or an out-of-range index, which the spec counts as an incorrect
-- assignment "anywhere", so the grouping is then never satisfied.
combineAny :: [Maybe Bool] -> Maybe Bool
combineAny rs
  | Nothing `elem` rs = Nothing
  | Just True `elem` rs = Just True
  | otherwise = Just False

-- | Maps each CompareOperator to its Haskell equivalent.
applyCompare :: CompareOperator -> Int -> Int -> Bool
applyCompare OpEq = (==)
applyCompare OpNeq = (/=)
applyCompare OpLt = (<)
applyCompare OpLe = (<=)
applyCompare OpGt = (>)
applyCompare OpGe = (>=)

-- | Applies an arithmetic operator to two already-evaluated
-- operands, failing only on division by zero.
applyOperator :: Operator -> Int -> Int -> Maybe Int
applyOperator Add a b = Just (a + b)
applyOperator Sub a b = Just (a - b)
applyOperator Mul a b = Just (a * b)
applyOperator Div _ 0 = Nothing
applyOperator Div a b = Just (a `div` b)
applyOperator Pow a b
  | b < 0 = Nothing
  | otherwise = Just (a ^ b)

-- | True once every constraint is definitely satisfied -- the
-- base-case check for a complete assignment.
allSatisfied :: ConstraintProblem -> Bool
allSatisfied p = all ((== Just True) . evalConstraint p) (searchConstraints p)

-- | The current time, formatted for use in a saved solution's
-- filename/header.
getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime

-- | Renders a solved binding list back into MiniZinc-Lite source,
-- with a generated-timestamp header, for writing to a solution file.
formatSolution :: String -> [Binding] -> String
formatSolution time bindings =
  unlines (("-- Generated: " ++ time) : "" : map formatBinding bindings)
  where
    formatBinding (name, value) = name ++ " = " ++ show value ++ ";"