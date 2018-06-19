#' Ustalanie parametru width dla funkcji geom_bar
#'
#' Funkcja pozwoli wyznaczyć optymalną szerokość słupków na wykresie w
#' zalezności od liczby kategorii zmiennej
#' @param n Wartość typu numeric reprezentująca liczbę kategorii, które
#' zostaną oznaczone na wykresie słupkowym.
#' @return Wartość typu numeric.
#' @examples
#' \dontrun{
#' n <- length(unique(df$type))
#' SettingWidth(n)}
#' @export
SettingWidth <- function(n = NULL) {
        if(!is.numeric(n))
                {stop("Zmienna n powinna być typu numeric")}
        if(length(n) != 1)
                {stop("Zmienna n powinna być pojedyczną liczbą")}
        if(n > 8)
                {stop("Zmienna n nie może być większa niż 8")}
        width <- 0.2+n*0.15
        return(width)
}
