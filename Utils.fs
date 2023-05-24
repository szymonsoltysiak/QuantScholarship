module Utils

let ofBool = function
  | true,a -> Some a
  | false,_ -> None


let tryParseCustomFormat (input: string) : bool * float =
  match System.Double.TryParse(input, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture) with
  | true, value -> true, value
  | _ -> false, 0.0