context("testowanie zmiennych na wejściu")

test_that("testowanie formatu zmiennej data",{
        x <- as.numeric(1:5)
        y <- as.numeric(20:24)
        xy <- c(39, 30, 90, 0, -12.999292)
        expect_that(CalculateDistance(xy, "x", "y"),
                throws_error("Argument data powinien być typu data.frame"))
})

test_that("testowanie formatu zmiennej lngtitude",{
        xy <- data.frame(c("k", 33.4, "a"), c(-3, 6.99, 90))
        names(xy) <- c("x", "y")
        expect_that(CalculateDistance(xy, "x", "y"),
                throws_error("Zmienna longtitude powinna być wartością liczbową"))
})

test_that("testowanie zakresu zmiennej lngtitude",{
        xy <- data.frame(c(-10, 33.4, 200), c(-3, 6.99, 90))
        names(xy) <- c("x", "y")
        expect_that(CalculateDistance(xy, "x", "y"),
                throws_error("Wartość longtitude powinna mieścić się w przedziale od -90 do 90"))
})

test_that("testowanie zakresu zmiennej lngtitude",{
        xy <- data.frame(c(-20, 33.4, 95, 1.9499), c(-3, 6.99, 180, -45.99997))
        names(xy) <- c("x", "y")
        expect_that(CalculateDistance(xy, "x", "y"),
                throws_error("Wartość longtitude powinna mieścić się w przedziale od -90 do 90"))
})

test_that("testowanie formatu zmiennej latitude",{
        xy <- data.frame(c(89, 33.4, 1), c("rty", 6.99, 90))
        names(xy) <- c("x", "y")
        expect_that(CalculateDistance(xy, "x", "y"),
                throws_error("Zmienna latitude powinna być wartością liczbową"))
})

test_that("testowanie zakresu zmiennej latitude",{
        xy <- data.frame(c(-90, 33.4, 18.97838292), c(-180.99827, 6.99, 90))
        names(xy) <- c("x", "y")
        expect_that(CalculateDistance(xy, "x", "y"),
                throws_error("Wartość latitude powinna mieścić się w przedziale od -180 do 180"))
})

test_that("testowanie zakresu zmiennej latitude",{
        xy <- data.frame(c(-20, 33.4, 90), c(-3, 6.99, 1000))
        names(xy) <- c("x", "y")
        expect_that(CalculateDistance(xy, "x", "y"),
                throws_error("Wartość latitude powinna mieścić się w przedziale od -180 do 180"))
})

context("testowanie rezultatów na wyjściu")

test_that("wynik jest wartością numeryczną",{
        xy <- data.frame(c(90, 28.9292992, -18.34, 0), c(178.993, 95, 0, -114.92928290023))
        names(xy) <- c("x", "y")
        expect_that(CalculateDistance(xy, "x", "y"), is_a("numeric"))
})
