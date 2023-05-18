module Utils

let ofBool = function
  | true,a -> Some a
  | false,_ -> None
