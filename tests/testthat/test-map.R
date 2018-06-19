context("testowanie zmiennych na wejściu")

test_that("testowanie formatu zmiennej name.type",{
        data(data)
        df$type <- as.factor(df$type)
        expect_that(Map(data = df, longtitude = "lng", latitude = "lat",
                name.type = "type"),
                throws_error("Argument name.type powinien być typu character"))
})

test_that("testowanie liczby kategorii w zmiennej name.type",{
        data(data)
        new.df <- df[, type := sample(x = letters[1:10], size = nrow(df),
                replace = TRUE)]
        expect_that(Map(data = new.df, longtitude = "lng", latitude = "lat",
                name.type = "type"),
                throws_error("Zmienna name.type może miec maksymanie 8 kategorii"))
})
