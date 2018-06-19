#' Wizualizacja geolokalizacji obiektów
#'
#' Plasowanie na mapie obiektów, których dane geolokalizacyjne zamieszczone
#' w ramce danych wraz z oznaczeniem ich rodzaju/ kategorii.
#'
#' @inheritDotParams SelectTypeDistance data longtitude latitude name.type
#' @return Tożsame z parametrem Value w [leaflet::leaflet]
#' @import data.table
#' @import leaflet
#' @examples
#' #Wizualizacja na mapie obiektów kulutry judaizmu w Krakowie położonych nie
#' #dalej niż 3km od Rynku Głównego
#' \dontrun{
#' new.df <- SelectTypeDistance(data = df, latitude = "lat",
#' longtitude = "lng", name.type = "type", sel.type = "Juish Culture",
#' sel.dist = 3)
#' Map(data = new.df, longtitude = "lng", latitude = "lat",
#' name.type = "type")}
#' @export
Map <- function(data = NULL, longtitude = NULL, latitude = NULL,
        name.type = NULL) {

        if(!is.character(data[[name.type]]))
                {stop("Argument name.type powinien być typu character")}

        show_types <- unique(data[[name.type]])
        n <- length(show_types)

        if(n > 8)
                {stop("Zmienna name.type może miec maksymanie 8 kategorii")}

        #Oznaczanie kolorem poszczególnych rodzajów kategorii zmiennej
        #name.tape na potrzeby wizualizacji
        choosingcolors <- c("blue", "violet","cyan", "darkorange", "green",
                "red", "yellow", "black")
        for (i in 1:n) {
                data<-data[get(name.type)==show_types[i],
                        col:=choosingcolors[i]]
        }

        #Wprowadzenie nazw zmiennych, które funkcja addCircleMarkers będzie          #potrafiła odczytać jako zmienne lng i lat
        names(data)[which(names(data)==longtitude)]<-"lng"
        names(data)[which(names(data)==latitude)]<-"lat"

        #Przygotowanie mapy
        map <- data %>% leaflet() %>%
        addTiles() %>%
        addMarkers(popup=data$name,clusterOptions=markerClusterOptions()) %>%         addCircleMarkers(color = data$col) %>%
        addLegend(labels = show_types, #= levels(data[[name.type]]),
                colors = choosingcolors[1:n],
                title="Legend", position="topleft")
        return(map)
}
