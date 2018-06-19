context("testowanie zmiennych na wejściu")

test_that("testowanie formatu zmiennej data",{
        df <- as.list(df)
        expect_that(ShowHowMany(data = df, name.type = "type"),
                throws_error("Argument data powinien być typu data.frame"))
})

test_that("testowanie formatu zmiennej name.type",{
        df <- df[, type:=as.factor(type)]
        expect_that(ShowHowMany(data = df, name.type = "type"),
                throws_error("Argument name.type powinien być typu character"))
})

test_that("testowanie liczby kategorii zmiennej name.type",{
        df <- df[, type:=sample(x = letters[1:12], size = nrow(df), replace = TRUE)]
        expect_that(ShowHowMany(data = df, name.type = "type"),
                throws_error("Zmienna name.type może mieć maksymanie 8 kategorii"))
})
