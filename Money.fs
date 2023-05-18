module Money

(* A type representing given amount of money in specific currency. Very bare bones, could be extended in various ways. Some examples:
1. Multiplication by float so that $1 * 100 = $100.
2. Addition to other Money instance so that $1 + $2 = $3, but 1 zl + $1 = <exception thrown> *)
type Money =
    {
        Value : float
        Currency : string
    }

    override this.ToString() = sprintf "%.2f (%s)" this.Value this.Currency
