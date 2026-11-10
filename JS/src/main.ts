import {
    fromEvent,
    map,
    merge,
    scan,
    startWith,
    switchMap,
    filter,
    type Observable,
} from "rxjs";
import { fromFetch } from "rxjs/fetch";
import { Action, Output, SaveStatus, State} from "./types";

const modelInput = document.getElementById(
    "model-input",
) as HTMLTextAreaElement;

const runButton = document.getElementById("run-model")!;
const saveButton = document.getElementById("save-model")!;

const outputEl = document.getElementById("model-output") as HTMLOutputElement;
const saveOutputEl = document.getElementById(
    "save-output",
) as HTMLOutputElement;

const initialState: State = {
    model: "",
    run: false,
    save: false,
};

const initialOutput: Output = {
    result: "",
    error: "",
};

const initialSaveStatus: SaveStatus = {
    message: "",
    error: "",
};

const modelInput$: Observable<Action<State>> = fromEvent(
    modelInput,
    "input",
).pipe(
    map(event => (event.target as HTMLTextAreaElement).value),
    map(value => state => ({
        ...state,
        model: value,
        run: false,
        save: false,
    })),
);

/**
 * Clicks on a button, as actions that set the run/save flags to the
 * given values. Shared by Run and Save, which differ only in the flags.
 */
function clickSets(
    button: HTMLElement,
    flags: Pick<State, "run" | "save">,
): Observable<Action<State>> {
    return fromEvent(button, "click").pipe(
        map(() => state => ({ ...state, ...flags })),
    );
}

const run$ = clickSets(runButton, { run: true, save: false });
const save$ = clickSets(saveButton, { run: false, save: true });

/**
 * POSTs the current model to a backend route and parses the JSON reply.
 * Shared by Run and Save, which differ only in the route and the reply.
 */
function postModel<T>(url: string, state: State): Observable<T> {
    const body = new URLSearchParams({ model: state.model });

    return fromFetch<T>(url, {
        method: "POST",
        headers: { "Content-Type": "application/x-www-form-urlencoded" },
        body: body.toString(),
        selector: res => res.json(),
    });
}

/** Runs the model on the backend and turns the reply into an Output. */
function getOutput(state: State): Observable<Action<Output>> {
    return postModel<Readonly<{ result: string } | { error: string }>>(
        "/api/minizinc",
        state,
    ).pipe(
        map(res => {
            if ("error" in res) {
                return () => ({ result: "", error: res.error });
            }
            return () => ({ result: JSON.stringify(res), error: "" });
        }),
    );
}

/** Asks the backend to save the solution; turns the reply into a status. */
function getSave(state: State): Observable<Action<SaveStatus>> {
    return postModel<Readonly<{ saved: string } | { error: string }>>(
        "/api/save",
        state,
    ).pipe(
        map(res => {
            if ("error" in res) {
                return () => ({ message: "", error: res.error });
            }
            return () => ({ message: `Saved to ${res.saved}`, error: "" });
        }),
    );
}

/**
 * The replies to one button: each time `wanted` holds for a new state,
 * send `request` and apply its reply to the current value, starting
 * from `initial`. Shared by the Run output and the Save status.
 */
function replies$<T>(
    state$: Observable<State>,
    wanted: (state: State) => boolean,
    request: (state: State) => Observable<Action<T>>,
    initial: T,
): Observable<T> {
    return state$.pipe(
        filter(wanted),
        switchMap(request),
        scan((current, action) => action(current), initial),
        startWith(initial),
    );
}

function main() {
    const state$ = merge(modelInput$, run$, save$).pipe(
        scan((state, action) => action(state), initialState),
        startWith(initialState),
    );
    state$.subscribe(console.log);

    const output$ = replies$(
        state$,
        state => state.run,
        getOutput,
        initialOutput,
    );

    output$.subscribe(output => {
        console.log(output);
        outputEl.value = output.error || output.result;
    });

    const saveStatus$ = replies$(
        state$,
        state => state.save,
        getSave,
        initialSaveStatus,
    );

    saveStatus$.subscribe(status => {
        console.log(status);
        saveOutputEl.value = status.error || status.message;
    });
}

if (typeof window !== "undefined") {
    window.addEventListener("load", main);
}