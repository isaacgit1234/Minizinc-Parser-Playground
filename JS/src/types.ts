export type State = Readonly<{
    model: string;
    run: boolean;
    save: boolean;
}>;

export type Output = Readonly<
    { result: string; error: "" } | { result: ""; error: string }
>;

/** What the Save button shows: the saved file's name, or an error. */
export type SaveStatus = Readonly<
    { message: string; error: "" } | { message: ""; error: string }
>;

export type Action<T> = (_: T) => T;
