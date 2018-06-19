#' Obliczanie dystansu obiektów geograficznych od centrum Krakowa
#'
#' Funkcja oblicza dystans (w km) od Centrum Krakowa (tj. Rynku Głównego)
#'
#' @param data Obiekt typu data.frame zawierający dane obiektów, w tym
#' położenie geografincze (tj. szerokość geograficzną - zmienna
#' \code{latitude} i długość geograficzna - zmienna \code{longtitude})
#' @param latitude Wektor wartości typu numeric reprezentujących szerokość
#' geograficzną wyrażoną w stopniach dziesiętnych, które wraz z
#' korespondującymi z nimi rekordami zmiennej \code{latitude} dają pełną
#' infomrację o współrzędnych geograficznych. Może przyjmować wartości
#' w zakresie od -90 do 90.
#' @param longtitude Wektor wartości typu numeric, reprezentujących długość
#' geograficzną wyrażoną w stopniach dziesiętnych, które wraz z
#' korespondującymi z nimi rekordami zmiennej \code{longtitude} dają pełną
#' infomrację o współrzędnych geograficznych. Może przyjmować wartości w
#' zakresie od -180 do 180.
#' @return Wektor wartości typu numeric, reprezetujących dystans obiektów
#' (w km) od centurm Krakowa (tj. Rynku Głównego).
#' @examples
#' \dontrun{
#' #Uzupełenianie bazy obiektów turystycznych Krakowa i okolic o informację
#' #dot.ich odległości od Rynku Głównego
#' new.df <- df[, distance:=CalculateDistance(df,"lng","lat")]}
#' @import data.table
#' @import sp
#' @export
CalculateDistance <- function(data, longtitude, latitude) {

        if(!is.data.frame(data))
                {stop("Argument data powinien być typu data.frame")}
        data <- data.table::data.table(data)
        if(!is.numeric(data[[longtitude]]))
                {stop("Zmienna longtitude powinna być wartością liczbową")}
        if(nrow(data[get(longtitude)> 90]) > 0)
                {stop("Wartość longtitude powinna mieścić się w przedziale od -90 do 90")}
        if(nrow(data[get(longtitude) < -90]) > 0)
                {stop("Wartość longtitude powinna mieścić się w przedziale od -90 do 90")}
        if(!is.numeric(data[[latitude]]))
                {stop("Zmienna latitude powinna być wartością liczbową")}
        if(nrow(data[get(latitude) > 180]) > 0)
                {stop("Wartość latitude powinna mieścić się w przedziale od -180 do 180")}
        if(nrow(data[get(latitude) < -180]) > 0)
                {stop("Wartość latitude powinna mieścić się w przedziale od -180 do 180")}

        lng_lat <- as.matrix(cbind(data[[longtitude]], data[[latitude]]))
        return(sp::spDistsN1(pts = lng_lat, pt = c(19.93676, 50.0619),
                longlat = TRUE))
}


