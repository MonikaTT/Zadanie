#' Zbiór danych dot. głównych obiektów turystycznych Krakowa i okolic
#'
#' Zawiera dane geolokalizacyjne subiektywnie wybranych obiektów
#' turystycznych
#'
#' @docType data
#'
#' @usage data(data)
#'
#' @format Ramka danych (data.frame) \code{df} w formacie data.table
#' zawierająca 119 rekordów i 4 zmienne:
#' \describe{
#'   \item{\code{name}}{nazwa obiektu, w języku angielskim}
#'   \item{\code{lat}}{szerokość geograficzna obiektu}
#'   \item{\code{lng}}{długość geograficzna obiektu}
#'   \item{\code{type}}{typ obiektu - kategoria oznaczona subiektywnie}
#' }
#'
#' @examples
#' \dontrun{
#' data(data)
#' str(df)}
#'
"df"
