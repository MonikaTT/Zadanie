context("testowanie zmiennych na wejściu")

test_that("testowanie typu argumentu name.type",{
        xy <- data.frame(c(24.6, 13.40032, -89.4, 0, 45.6, 1:3),
                c(180, -180, 8.29910100, 0, 10.9086, 1:3),
                1:8)
        names(xy) <- c("lng", "lat", "type")
        expect_that(SelectTypeDistance(data = xy, longtitude = "lng",
                latitude = "lat", name.type = "type", sel.type = "1"),
                throws_error("Argument name.type powinien być typu character"))
})

context("testowanie rezultatów na wyjściu")

test_that("wynik jest obiektem data.table",{
        xy<-data.frame(1:5, (10:14), c("raz", "raz", "dwa", "dwa", "dwa"))
        names(xy) <- c("lng", "lat", "type")
        xy$type <- as.character(xy$type)
        expect_that(SelectTypeDistance(data = xy, longtitude = "lng",
                latitude = "lat", name.type = "type", sel.type = "raz"),
                is_a("data.table"))
})
