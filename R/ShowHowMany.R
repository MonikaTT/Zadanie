#' Wizualizacja liczby obiektów
#'
#' Funkcja generuje wykres słupkowy prezentacjący liczbę poszczególnych rodzajów
#' obiektów znajdujących się w ramce danych
#' @param data Obiekt typu data.frame zawierający przynajmniej jedną zmienną
#' typu character, reprezentującą kategorie typu obiektu.
#' @param name.type Nazwa zmiennej w ramce danych \code{data} zawierająca
#' kategorie typu obiektu. Zmienna typu character.
#' @import ggplot2
#' @import data.table
#' @return Wartość typu numeric, do wykorzystania jako parametr width
#' funkncji geom_bar.
#' @examples
#' \dontrun{
#' new.df <- SelectTypeDistance(data = df, latitude = "lat", longtitude = "lng",
#' name.type = "type", sel.type = c("Streets and Squares", "Parks and Nature"),
#' sel.dist = 20)
#' ShowHowMany(data = new.df, name.type = "type")}
#' @export
ShowHowMany <- function(data = NULL, name.type = NULL) {

        if(!is.data.frame(data))
                {stop("Argument data powinien być typu data.frame")}
        if(!is.character(data[[name.type]]))
                {stop("Argument name.type powinien być typu character")}

        #Przygotowanie tabeli danych na potrzeby wykresu
        ile <- data.table::data.table(data[[name.type]])
        data.table::setkey(ile, V1)
        ile <- ile[, n:=.N, by=V1]
        ile <- unique(ile)

        #Weryfikacja liczby kategorii obiektów (nn), które zostana pokazane
        #na wykresie. Posłuży także ustaleniu parametru width w funkcji geom_bar
        nn <- nrow(ile)
        if(nn > 8)
                {stop("Zmienna name.type może mieć maksymanie 8 kategorii")}

        #Ustalanie porządku malejącego w tabeli na potrzeby wykresu
        ile <- ile[, V1:=as.factor(V1)]
        ile <- ile[, V1:= factor(V1, levels(V1)[order(n)])]

        plot <- ggplot2::ggplot(data = ile, mapping = ggplot2::aes(x = V1, y = n)) +
                ggplot2::geom_bar(stat = "identity", width = SettingWidth(nn),
                        fill = "steelblue") +
                ggplot2::labs(title="Liczba poszczególnych rodzajów obiektów w bazie",
                        size = 12) +
                ggplot2::geom_text(ggplot2::aes(label = n), hjust = -0.5, size = 4)
        plot <- plot + ggplot2:: theme (
                        plot.title = ggplot2::element_text(face = "bold"),
                        axis.title.x = ggplot2::element_blank(),
                        axis.text.x = ggplot2::element_blank(),
                        axis.ticks.x=ggplot2::element_blank(),
                        axis.title.y =ggplot2::element_blank(),
                        axis.text.y= ggplot2::element_text(size = 11))
        plot <- plot + ggplot2::coord_flip()
        plot
}
