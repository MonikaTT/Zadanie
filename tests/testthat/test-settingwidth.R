context("testowanie zmiennych na wejściu")

test_that("testowanie formatu zmiennej n",{
        n <- "a"
        expect_that(SettingWidth(n),
                throws_error("Zmienna n powinna być typu numeric"))
})

test_that("testowanie wymiarów zmiennej n",{
        n <- as.data.frame(cbind(0, 1.233, 9))
        expect_that(SettingWidth(n),
                throws_error("Zmienna n powinna być typu numeric"))
})

test_that("testowanie wymiarów zmiennej n",{
        n <- c(0, 1.233, 9)
        expect_that(SettingWidth(n),
                throws_error("Zmienna n powinna być pojedyczną liczbą"))
})

test_that("testowanie wartości zmiennej n",{
        n <- 10
        expect_that(SettingWidth(n),
                throws_error("Zmienna n nie może być większa niż 8"))
})

test_that("wynik jest wartością numeryczną",{
        n <- 7
        expect_that(SettingWidth(n), is_a("numeric"))
})


