module FSParser
    open System.Linq
    open System.Collections.Generic
    type Pattern =
    | Char of char
    | String of string
    | NChar of char
    | NString of string
    | And of Pattern list
    | Or of Pattern list
    | For0 of Pattern
    | For1 of Pattern
    | Or01 of Pattern
    | Input of string
    | NUMBER
    | STRING
    | OP
    | Ref of Pattern
    let rec Join(join: string, array: char[]) =
        let mutable result = ""
        let mutable count = 0
        for item in array do
            result <- result + item.ToString()
            count <- count + 1
            if count < array.Length then
                result <- result + join
        result
    let Code: Dictionary<string, Pattern> = new Dictionary<string, Pattern>()
    let Op: List<string> = new List<string>()
    let rec Add(name: string, code: Pattern) =
        Code.Add(name, code)
    let rec OpAdd(operator: string) =
        Op.Add(operator)
    let rec OpesAdd(opes: string list) =
        for item in opes do
            Op.Add(item)
    let rec IsMatch(s: string) =
        let mutable S = s
        let mutable S0 = S
        let rec For(pattern: Pattern, mode: int) =
            match pattern with
            | Char ch ->
                if S.StartsWith(ch) then
                    if mode = 0 then S <- Join("", S.Skip(1).ToArray())
                    true
                else
                    false
            | String st ->
                if S.StartsWith(st) then
                    if mode = 0 then S <- Join("", S.Skip(st.Length).ToArray())
                    true
                else false
            | NChar ch ->
                if S.StartsWith(ch) = false then
                    if mode = 0 then S <- Join("", S.Skip(1).ToArray())
                    true
                else false
            | NString st ->
                if S.StartsWith(st) = false then
                    if mode = 0 then S <- Join("", S.Skip(st.Length).ToArray())
                    true
                else false
            | And hs ->
                let bool: List<bool> = new List<bool>()
                for item in hs do
                    if bool.Count = 0 || bool.All(fun x -> x = true) then bool.Add(For(item, mode))
                bool.All(fun x -> x = true)
            | Or hs ->
                let bool: List<bool> = new List<bool>()
                let s0 = S
                for item in hs do
                    if bool.Any(fun x -> x = true) = false then
                        bool.Add(For(item, mode))
                    if bool.Any(fun x -> x = true) = false then S <- s0
                bool.Any(fun x -> x = true)
            | For0 hs ->
                while For(hs, mode) do
                    printf ""
                true
            | For1 hs ->
                let mutable count = 0
                while For(hs, mode) do
                    count <- count + 1
                if count <> 0 then true
                else false
            | Or01 hs ->
                let _ = For(hs, mode)
                true
            | Input pat ->
                For(Code[pat], mode)
            | NUMBER ->
                For(For1(Or([String "0"; String "1"; String "2"; String "3"; String "4"; String "5"; String "6"; String "7"; String "8"; String "9"])), 0)
            | STRING ->
                For(For1(Or([NChar '0'; NChar '1'; NChar '2'; NChar '3'; NChar '4'; NChar '5'; NChar '6'; NChar '7'; NChar '8'; NChar '9'; NChar '\"'])), 0)
            | OP ->
                let mutable bool: List<bool> = new List<bool>()
                for _ in Op do
                    bool.Add(false)
                let mutable count = 0
                let mutable op = ""
                while count < Op.Count do
                    if bool.Any(fun x -> x = true) = false then bool[count] <- For(String Op[count], 1)
                    if bool.Any(fun x -> x = true) && op = "" then op <- Op[count]
                    count <- count + 1
                S <- Join("", S.Skip(op.Length).ToArray())
                bool.Any(fun x -> x = true)
            | Ref hs -> For(hs, 0)
        let mutable bool: bool = false
        let mutable bool0: bool = true
        let mutable count = 0
        while count < Code.Count do
            if bool0 then
                bool <- For(Code.Values.ToArray()[count], 0)
                if bool && S.Length = 0 then
                    bool0 <- false
                else
                    bool <- false
                    S <- S0
            count <- count + 1
        bool
    let after: Dictionary<string, string> = new Dictionary<string, string>()
    let rec After(name: string, replace: string) =
        after.Add(name, replace)
    let rec Parse(s: string) =
        let mutable S = s
        let mutable S0 = S
        let mutable result = ""
        let mutable pat = ""
        let rec For(pattern: Pattern, mode: int) =
            match pattern with
            | Char ch ->
                if S.StartsWith(ch) then
                    if mode = 0 then
                        S <- Join("", S.Skip(1).ToArray())
                    ch.ToString()
                else ""
            | String st ->
                if S.StartsWith(st) then
                    if mode = 0 then
                        S <- Join("", S.Skip(st.Length).ToArray())
                    st
                else ""
            | NChar ch ->
                if S.StartsWith(ch) = false then
                    let s0 = S.Take(1).ToString()
                    if mode = 0 then
                        S <- Join("", S.Skip(1).ToArray())
                    s0
                else ""
            | NString st ->
                if S.StartsWith(st) = false then
                    let s0 = S.Take(st.Length).ToString()
                    if mode = 0 then S <- Join("", S.Skip(st.Length).ToArray())
                    s0
                else ""
            | And hs ->
                let mutable str: string = ""
                let mutable count = 0
                for item in hs do
                    let s0 = For(item, 0)
                    if s0 <> "" then count <- count + 1
                    str <- str + s0
                if count = hs.Length then str
                else ""
            | Or(hs: Pattern list) ->
                let str: List<string> = new List<string>()
                str.Add ""
                let s0 = S
                for item in hs do
                    if str.Any(fun x -> x <> "") = false then str.Add(For(item, 0))
                    if str.Any(fun x -> x <> "") = false then S <- s0
                if str.Where(fun x -> x <> "").Count() = 0 then
                    ""
                else str.Where(fun x -> x <> "").First()
            | For0 hs ->
                let mutable str = ""
                let mutable sub = "1"
                while sub <> "" do
                    sub <- For(hs, 0)
                    str <- str + sub
                str
            | For1 hs ->
                let mutable count = 0
                let mutable str = ""
                let mutable sub = "1"
                while sub <> "" do
                    sub <- For(hs, 0)
                    str <- str + sub
                    count <- count + 1
                if count <> 0 then str
                else ""
            | Or01 hs ->
                let s0 = For(hs, 0)
                s0
            | Input pat ->
                let s = For(Code[pat], 0)
                if mode = 2 then printfn "%s" s
                s
            | NUMBER ->
                For(For1(Or([String "0"; String "1"; String "2"; String "3"; String "4"; String "5"; String "6"; String "7"; String "8"; String "9"])), 0)
            | STRING ->
                For(For1(Or([NChar '0'; NChar '1'; NChar '2'; NChar '3'; NChar '4'; NChar '5'; NChar '6'; NChar '7'; NChar '8'; NChar '9'; NChar '\"'])), 0)
            | OP ->
                let mutable str: string = ""
                let mutable count = 0
                let mutable op = ""
                while count < Op.Count do
                    if str = "" then str <- For(String Op[count], 1)
                    if str <> "" && op = "" then op <- Op[count]
                    count <- count + 1
                S <- Join("", S.Skip(op.Length).ToArray())
                op
            | Ref hs ->
                let s0 = For(hs, 0)
                if after.Keys.Contains(pat) && s0 <> "" then result <- result + after[pat].Replace("%%", s0)
                s0
        for item in Code.Keys do
            pat <- item
            let s0 = For(Code[item], 2)
            if s0 = "" then
                S <- S0
        result