#' Selekcja obiektów
#'
#' Pozwala na selekcjonowane obiektów ramki danych w oparciu o kryterium typu
#' obiektu oraz odległości, która dzieli je od centrum Krakowa (tj. Rynku
#' Głównego).
#'
#' @inheritParams CalculateDistance
#' @param name.type Nazwa zmiennej w ramce danych \code{data} zawierająca
#' kategorie typu obiektu. Zmienna typu character.
#' @param sel.type Wybrana kategoria typu obiektu (mieszcząca się w zmiennej
#' \code{name.type}), w oparciu o którą będzie filtrowana ramka danych
#' \code{data}.
#' @param sel.dist Wartość numeryczna (numeric). Kryterium wyboru obiektów
#' określające maksymalna odległość obiektu (w km) od centrum Krakowa. Domyślnie
#' wynosi 60km.
#' @return Obiekty typu data.frame.
#' @import data.table
#' @examples
#' #Wybór z bazy obiektów turystycznych Krakowa wyłącznie muzeów i kościołów
#' #położonych nie dalej niż 2km od Rynku Głównego
#' \dontrun{
#' new.df<-SelectTypeDistance(data = df, latitude = "lat",
#' longtitude = "lng", name.type = "type",
#' sel.type = c("Churches", "Museums"), sel.dist = 2)
#' }
#' @export
SelectTypeDistance <- function(data = NULL, latitude = NULL,
        longtitude = NULL, name.type = NULL, sel.type = NULL, sel.dist = 60){

        if(!is.character(data[[name.type]]))
                {stop("Argument name.type powinien być typu character")}

        data <- data.table::data.table(data)
        #Wyznaczanie dystansu obiektu od centrum Krakowa
        data <- data[, distance:=CalculateDistance(data = data,
                longtitude = longtitude,
                latitude = latitude)]
        #Filtrowanie zmiennych
        return(data[get(name.type) %in% sel.type & distance <= sel.dist])
}

