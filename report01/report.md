Filip Pawłowicz, 414324

# Regresja liniowa i interakcje

## Zbiór danych Real Estate Valuation.

Podstawowym element pracy z danymi jest ich analiza i selekcja cech. Na początku usunąłem kolumnę zawierającą indeksy - do regresji jest to zbędne. Zmieniłem także nazwy kolumn na prostsze i krótsze.


```r
df <- read_excel("real-estate.xlsx")
dim(df)

head(df)
summary(df)

df$No <- NULL
names(df) <- c("date", "age", "dist_mrt", "n_stores", "lat", "lon", "price")

head(df)
```

Kolejnym krokiem jest sprawdzenie wartości brakujących lub niepoprawnych. Można to bardzo prosto sprawdzić poleceniem `colSums(is.na(df))`. Ten zbiór danych nie zawiera takich wartości, co wskazali również autorzy zbioru. W ogólnósci brak danych w kolumnie możemy naprawić, podstawiając w te miejsca średnią wartość lub mediane. Jeśli w danej kolumnie takich przypadkach jest więcej, to raczej skłaniamy się do usunięcia tej cechy (lub wierszy).

W modelach liniowych dane nie powinny być silnie skorelowane. Poniżej przedstawiam macierz korelacji oraz stworzoną na jej podstawie tzw. heatmap.

```r
cor_matrix <- cor(df)
print(round(cor_matrix, 2))

corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black")
```

```r
          date   age dist_mrt n_stores   lat   lon price
date      1.00  0.02     0.06     0.01  0.04 -0.04  0.09
age       0.02  1.00     0.03     0.05  0.05 -0.05 -0.21
dist_mrt  0.06  0.03     1.00    -0.60 -0.59 -0.81 -0.67
n_stores  0.01  0.05    -0.60     1.00  0.44  0.45  0.57
lat       0.04  0.05    -0.59     0.44  1.00  0.41  0.55
lon      -0.04 -0.05    -0.81     0.45  0.41  1.00  0.52
price     0.09 -0.21    -0.67     0.57  0.55  0.52  1.00
```

![heatmap](images/heatmap-real-estate.png)

Na przedstawionym obrazie widać dużą korelację między longitude a odległością do najbliższej stacji metra. Zbiór danych dotyczy miasta New Taipei, którego centrum zlokalizowane jest bardziej na zachodzie - tam też skupia się większość stacji.

```r
plot(df$lon, df$dist_mrt, main = "longitude vs dist_mrt",
     xlab = "longitude", ylab = "dist_mrt", col = "steelblue", pch = 16)
abline(lm(dist_mrt ~ lon, data = df), col = "red", lwd = 2)
```

![longitude vs dist_mrt](images/real-estate-longitude-vs-dist.png)

Testem, który służy do sprawdzenia liniowej korelacji danych jest Test Pearsona. W naszym przypadku test zwraca zależność istotnie statystyczną dla podanych cech, ale tylko dla `dist_mrt` i `lon` ta korelacja jest tak duża.

```r
cor.test(df$dist_mrt, df$price)
cor.test(df$n_stores, df$price)
cor.test(df$lat, df$lon)
cor.test(df$dist_mrt, df$lon)
```

Poniżej przedstawiam też przykładowo wykres ceny od odległości do stacji metra. Podobne wykresy wykonałem dla pozostałych cech.
![price vs dist_mrt](images/real-estate-price-vs-dist.png)

Kolejnym niepożądanym zjawiskiem w modelach liniowych są obserwacje odstające. Wynika to z zastosowania MSE jako funkcji kosztu, która silnie karze duże błędy. W praktyce takie obserwacje powinny być usuwane z danych lub odpowiednio ograniczane, np. poprzez winsoryzację, czyli przycinanie wartości skrajnych do ustalonych progów. Zastosowałem drugą opcję. Poniżej przedstawiam kod do stworzenia wykresów pudełkowów oraz jeden przykładowy.

```r
boxplot(df$price, main = "price", col = "steelblue")
boxplot(df$dist_mrt, main = "dist_mrt", col = "steelblue")
boxplot(df$age, main = "age", col = "steelblue")
boxplot(df$n_stores, main = "age", col = "steelblue")
```

![box](images/real-estate-box-dist-mrt.png)
Na powyższym wykresie pudełkowych widać wiele wartości odstających - są domy, które znajdują się bardzo daleko od stacji metra.


```r
winsorize <- function(x) {
  q      <- quantile(x, c(0.25, 0.75))
  iqr    <- q[2] - q[1]
  lower  <- q[1] - 1.5 * iqr
  upper  <- q[2] + 1.5 * iqr
  pmax(pmin(x, upper), lower)
}

df$price    <- winsorize(df$price)
df$dist_mrt <- winsorize(df$dist_mrt)
df$age      <- winsorize(df$age)
```

Stwórzmy teraz model liniowy, sprawdźmy wpływ zmiennych na niego oraz założenia regresji.

```r
model_multi <- lm(price ~ ., data = df)
summary(model_multi)

model_interact <- lm(price ~ dist_mrt * n_stores + dist_mrt * lat + age + date, data = df)
summary(model_interact)

AIC(model_multi, model_interact)
```

Na podstawie kryterium AIC widać, że model z interakcjami troszkę lepiej wyjaśnia dane. Będę go stosować w dalszej analizie. Liniowość sprawdzam wykresem Residuals vs Fitted - reszty powinny być losowo rozrzucone wokół poziomej lini zero, bez żadnego wzorca. U mnie widać lekką krzywą U - powoduje to prawdopodobnie zmienna dist_mrt, która zmienia się dość gwałtownie. Do sprawdzenia autokorelacji cech używam Testu Durbina-Watsona, który sprawdza, czy kolejne reszty są ze sobą skorelowane. Wynik DW jest bliski 2, co oznacza break autokorelacji. Homoskedastyczność testem Breuscha-Pagana - p > 0.05, co oznacza wariancję stałą. Testem Shapiro-Wilka sprawdzam, czy reszty mają rozkład normalny - p < 0.05, więc nie mają.

```r
plot(model_interact, which = 1)

dwtest(model_interact)

bptest(model_interact)

shapiro.test(residuals(model_interact))

```

```r
> dwtest(model_interact)

	Durbin-Watson test

data:  model_interact
DW = 2.1413, p-value = 0.9258
alternative hypothesis: true autocorrelation is greater than 0

> bptest(model_interact)

	studentized Breusch-Pagan test

data:  model_interact
BP = 13.287, df = 7, p-value = 0.06541

> shapiro.test(residuals(model_interact))

	Shapiro-Wilk normality test

data:  residuals(model_interact)
W = 0.92943, p-value = 4.543e-13
```


![Residuals vs fitted](images/real-estate-residuals-vs-fitted.png)

# Regresja Poissonowska

## Funkcja wiarygodności dla regresji Poissonowskiej


Dla pojedynczej obserwacji $y_i$, prawdopodobieństwo że przyjmie ona wartość $y_i$ wynosi:
$$P(Y_i = y_i) = \frac{\lambda_i^{y_i} \cdot e^{-\lambda_i}}{y_i!}$$
gdzie $\lambda_i$ to oczekiwana liczba zdarzeń dla obserwacji $i$.


Zakładamy że wszystkie $n$ obserwacji są niezależne, więc łączne prawdopodobieństwo to iloczyn:
$$L(a_0, a_1, \ldots, a_p) = \prod_{i=1}^{n} \frac{\lambda_i^{y_i} \cdot e^{-\lambda_i}}{y_i!}$$

Z założenia:

$$\log(\lambda_i) = a_0 + a_1 X_{i1} + \cdots + a_p X_{ip}$$

czyli:

$$\lambda_i = e^{a_0 + a_1 X_{i1} + \cdots + a_p X_{ip}}$$


$$L(a_0, \ldots, a_p) = \prod_{i=1}^{n} \frac{\left(e^{a_0 + a_1 X_{i1} + \cdots + a_p X_{ip}}\right)^{y_i} \cdot e^{-e^{a_0 + a_1 X_{i1} + \cdots + a_p X_{ip}}}}{y_i!}$$


Maksymalizujemy logarytm funkcji wiarygodności:

$$\ell(a_0, \ldots, a_p) = \sum_{i=1}^{n} \left[ y_i \cdot (a_0 + a_1 X_{i1} + \cdots + a_p X_{ip}) - e^{a_0 + a_1 X_{i1} + \cdots + a_p X_{ip}} - \log(y_i!) \right]$$

Niech $\eta_i = a_0 + a_1 X_{i1} + \cdots + a_p X_{ip}$, otrzymujemy postać skróconą:

$$\ell = \sum_{i=1}^{n} \left[ y_i \cdot \eta_i - e^{\eta_i} - \log(y_i!) \right]$$

Czynnik $\log(y_i!)$ można pominąć - nie zależy od parametrów.

```r
df <- read.table("poisson.data", header = TRUE, sep = ";", dec = ",")

head(df)
summary(df)
dim(df)


likelihood <- function(params, y, x) {
  a0     <- params[1]
  a1     <- params[2]
  eta    <- a0 + a1 * x
  lambda <- exp(eta)
  nll    <- -sum(y * eta - lambda)
  return(nll)
}

names(df)
y <- df$Y
x <- df$X1

result <- optim(par = c(0, 0), fn = likelihood, y = y, x = x)

a0_optim <- result$par[1]
a1_optim <- result$par[2]

cat("a0 (intercept):", a0_optim, "\n")
cat("a1 (slope)    :", a1_optim, "\n")

model_poisson <- glm(y ~ x, data = df, family = poisson)
summary(model_poisson)
```

a) Deviance Residuals to taki odpowiednik reszt z regresji liniowej, ale dla modeli GLM. Mierzy, jak bardzo każda z obserwacji odstaje od wartości przewidywanej przez model. Liczy się to trochę bardziej skomplikowanym wzorem. W moim p rzypadku prawie wszystkie wartości mieszczą się w przedziale (-2, 2), co oznacza, że model jest dobrze dopasowany.

```r
dev_res <- residuals(model_poisson, type = "deviance")
summary(dev_res)

  Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
-2.14713 -0.69081  0.08003 -0.02299  0.55972  2.80599
```

b) Z podsumowania modelu widać, że oba współczynniki mają '***', więc są istotne. Wartości współczynników są takie same, jak te uzyskane przez funkcję optim, gdyż optymalizują tę samą funkcję wiarygodności, róznią się tylko algorytmem.

```
glm(formula = y ~ x, family = poisson, data = df)

Coefficients:
            Estimate Std. Error z value Pr(>|z|)
(Intercept) 1.032997   0.051017   20.25   <2e-16 ***
x           0.497547   0.006194   80.33   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```

c) Null Deviance to przewidywanie każdej obserwacji tylko średnią ze wszystkich $y$, bez brania pod uwagę predykatora $x$. Residual deviance to już przewidywanie z braniem predykatora. W moim przypadku residual deviance jest znacznie mniejszy od null deviance, co oznacza, że zmienna $x$ bardzo dużo wyjaśnia.

```r
Null deviance: 9945.17  on 99  degrees of freedom
Residual deviance:  101.89  on 98  degrees of freedom
```

# Ships Data

W tym przypadku będe modelować częstosć wypadków, czyli liczbę wypadków na jednostkę czasu, a nie samą liczbę wypadków, ze względu na zmienną objaśniająca `period`. Gdy statek jest dłużej eksploatowany, to może mieć więcej wypadków. `period` będzie stanowić tzw. offset w regresji Poissonowskiej.

Zwróćmy też uwage, że zmienne year, period, type to tak naprawdę zmienne kategoryczne, a model potraktuje je jako wartości ciągłe. Wartości tej cechy to po prostu kategorie: 65 oznacza pewien zakres (65-70), a nie konkretną liczbę. W tym celu zamieniamy te zmienne na wartości kategoryczne, stosując One Hot Encoding.

```r
library(MASS)
data("ships")

head(ships)
summary(ships)
str(ships)

names(ships)

ships$type   <- factor(ships$type)
ships$year   <- factor(ships$year)
ships$period <- factor(ships$period)

ships_df <- subset(ships, service > 0)

model_ships <- glm(incidents ~ type + year + period + offset(log(service)), data = ships_df, family = poisson)

summary(model_ships)
```
```r
> summary(ships)
 type  year    period     service          incidents
 A:8   60:10   60:20   Min.   :    0.0   Min.   : 0.0
 B:8   65:10   75:20   1st Qu.:  175.8   1st Qu.: 0.0
 C:8   70:10           Median :  782.0   Median : 2.0
 D:8   75:10           Mean   : 4089.3   Mean   : 8.9
 E:8                   3rd Qu.: 2078.5   3rd Qu.:11.0
                       Max.   :44882.0   Max.   :58.0
```

Z podsumowania modelu widać, że program poprawnie potraktował year i period jako zmienne kategoryczne (osobne zmienne).
```r
> summary(model_ships)

Call:
glm(formula = incidents ~ type + year + period + offset(log(service)),
    family = poisson, data = ships_df)

Coefficients:
            Estimate Std. Error z value Pr(>|z|)
(Intercept) -6.40590    0.21744 -29.460  < 2e-16 ***
typeB       -0.54334    0.17759  -3.060  0.00222 **
typeC       -0.68740    0.32904  -2.089  0.03670 *
typeD       -0.07596    0.29058  -0.261  0.79377
typeE        0.32558    0.23588   1.380  0.16750
year65       0.69714    0.14964   4.659 3.18e-06 ***
year70       0.81843    0.16977   4.821 1.43e-06 ***
year75       0.45343    0.23317   1.945  0.05182 .
period75     0.38447    0.11827   3.251  0.00115 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for poisson family taken to be 1)

    Null deviance: 146.328  on 33  degrees of freedom
Residual deviance:  38.695  on 25  degrees of freedom
AIC: 154.56

Number of Fisher Scoring iterations: 5
```

```r
dev_res <- resid(model_ships, type = "deviance")

hist(dev_res, breaks = 20, col = "steelblue",
     main = "Deviance Residuals - ships",
     xlab = "Value")
abline(v = 0, col = "red", lwd = 2)

qqnorm(dev_res, main = "Q-Q plot residuals")
qqline(dev_res, col = "red", lwd = 2)

shapiro.test(dev_res)
```

```r
	Shapiro-Wilk normality test

data:  dev_res
W = 0.94061, p-value = 0.06427
```

![deviance residuals](images/ships-deviance-residuals.png)

![qq plot](images/qq-plot.png)

Tak, jest spełnione załóżenie reszt dot. ich normalności, co potwierdza Test Shapiro-Wilka.

# Esoph

Podobnie, jak powyżej, mamy doczynienia z danymi kategorycznymi oraz zmienną `ncontrols` - liczba osób zdrowych, która będzie służyć, jako offset. Przekształciłem je na odpowiednie typy, dokonałem też pewnej selekcji i inżynieri cech, biorąc tylko wiersze z `ncases` i `ncontrols`, które są dodatnie.

```r
library(ISwR)
data("esoph")

esoph_df <- esoph

head(esoph_df)
summary(esoph_df)
sum(esoph_df$ncontrols == 0)
sum(is.na(esoph_df$ncontrols))

esoph_df$agegp <- factor(esoph_df$agegp, ordered = FALSE)
esoph_df$alcgp <- factor(esoph_df$alcgp, ordered = FALSE)
esoph_df$tobgp <- factor(esoph_df$tobgp, ordered = FALSE)

esoph_df <- subset(esoph_df, ncontrols > 0)
esoph_df <- subset(esoph_df, ncases > 0)

model_esoph <- glm(ncases ~ agegp + alcgp + tobgp + offset(log(ncontrols)), data = esoph_df, family = poisson)

summary(model_esoph)
```

```r
glm(formula = ncases ~ agegp + alcgp + tobgp + offset(log(ncontrols)),
    family = poisson, data = esoph_df)

Coefficients:
            Estimate Std. Error z value Pr(>|z|)
(Intercept)  -3.3022     0.4119  -8.016 1.09e-15 ***
agegp45-54    0.5943     0.3867   1.537   0.1243
agegp55-64    1.0250     0.3660   2.801   0.0051 **
agegp65-74    1.5020     0.3775   3.979 6.92e-05 ***
agegp75+      1.3086     0.5302   2.468   0.0136 *
alcgp40-79    1.1816     0.2289   5.162 2.44e-07 ***
alcgp80-119   1.6485     0.2534   6.504 7.81e-11 ***
alcgp120+     2.6987     0.2730   9.885  < 2e-16 ***
tobgp10-19    0.3503     0.1843   1.900   0.0574 .
tobgp20-29    0.4985     0.2210   2.255   0.0241 *
tobgp30+      1.5767     0.2642   5.968 2.40e-09 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for poisson family taken to be 1)

    Null deviance: 221.899  on 46  degrees of freedom
Residual deviance:  54.769  on 36  degrees of freedom
AIC: 218.06

Number of Fisher Scoring iterations: 5
```

Na podstawie podsumowania modelu widać, że niektóre zmienne nie są istotne. Istotnymi zmiennymi są min. 'agegp65', 'alcgp40', 'alcgp80', 'alcgp120' i 'tobgp30'. Stworzę teraz model, biorąc tylko najistotniejsze zmienne.

```r
model_esoph2 <- glm(ncases ~ alcgp + tobgp + offset(log(ncontrols)), data = esoph_df, family = poisson)

summary(model_esoph2)

AIC(model_esoph, model_esoph2)
```

```r
> AIC(model_esoph, model_esoph2)
             df      AIC
model_esoph  11 218.0587
model_esoph2  7 238.6873
```

AIC pełnego modelu jest mniejszy, co oznacza, że model pełny jest lepszy. Oznacz to, mimo, że niektóre poziomy 'agegp' są nieostotne, zmienna jako całość poprawia model - jej usunięcie pogarsza dopasowanie.

```r
dev_res <- resid(model_esoph, type = "deviance")
summary(dev_res)
shapiro.test(dev_res)
```

```r
> summary(dev_res)
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
-2.093117 -0.870512 -0.005934  0.051550  0.845322  2.312533
>
> shapiro.test(dev_res)

	Shapiro-Wilk normality test

data:  dev_res
W = 0.97393, p-value = 0.3708
```

Deviance Residuals przyjmują wartości w przedziale (-2.09, 2.31), czyli wszystkie mieszczą się w bezpiecznym przedziale (-3, 3). Mediana i średnia są praktycznie równe zero, co świadczy o symetrycznym rozkładzie reszt bez wyraźnego wzorca.

Test Shapiro-Wilka dał wynik W = 0.974, p = 0.371. Ponieważ p > 0.05, nie ma podstaw do odrzucenia hipotezy zerowej o normalności reszt - reszty mają rozkład normalny.

Oba wyniki potwierdzają że model jest dobrze dopasowany do danych.

# CLM Data

```r
library(ordinal)

df <- read.csv("clm.data", header = TRUE)

df$id <- NULL
df$satysfakcja <- factor(df$satysfakcja, ordered = TRUE)

str(df)
summary(df)

model_clm <- clm(satysfakcja ~ cena + obsluga, data = df)

summary(model_clm)
```

Wyniki prezentują się następująco. Okazuje się, że żadna zmienna nie jest istotna (obie p > 0.05). Oznacza to, że ani cena, ani obsługa nie mają istotnego wpływu na poziom satysfakcji. Wagi są bliskie zero.
```r
> summary(df)
 satysfakcja      cena           obsluga
 1:21        Min.   : 50.95   Min.   : 1.00
 2:20        1st Qu.: 92.07   1st Qu.: 3.00
 3:23        Median :127.36   Median : 5.00
 4:17        Mean   :127.81   Mean   : 5.53
 5:19        3rd Qu.:159.07   3rd Qu.: 9.00
             Max.   :197.85   Max.   :10.00

> summary(model_clm)
formula: satysfakcja ~ cena + obsluga
data:    df

 link  threshold nobs logLik  AIC    niter max.grad cond.H
 logit flexible  100  -159.66 331.31 4(0)  1.69e-12 1.2e+06

Coefficients:
         Estimate Std. Error z value Pr(>|z|)
cena    -0.005178   0.004391  -1.179    0.238
obsluga  0.014197   0.058268   0.244    0.808

Threshold coefficients:
     Estimate Std. Error z value
1|2 -1.926818   0.748672  -2.574
2|3 -0.953354   0.730469  -1.305
3|4 -0.001363   0.719511  -0.002
4|5  0.879775   0.725654   1.212
```

Widać także progi ('threshold coefficients'). Są to granice między kolejnymi kategoriami satysfakcji. Podsumowując, model jest słabo dopasowany - żadna zmienna nie jest istotna.

# Wine data

```r
data(wine, package = "ordinal")

head(wine)
summary(wine)

model_wine <- clm(rating ~ response + temp + contact, data = wine)
summary(model_wine)

cor(as.numeric(wine$rating), wine$response)

model_wine <- clm(rating ~ temp + contact, data = wine)
summary(model_wine)
```

Model zbudowałem na atrybutach `temp` i `contact` - pominięto `response`, ponieważ jest silnie skorelowany z `rating` (rating to skategoryzowana wersja response), co powodowało osobliwość macierzy Hessiana i problemy z estymacją.

Obie zmienne okazały się istotne. `tempwarm` oznacza że ciepłe wino ma znacznie wyższe prawdopodobieństwo uzyskania wyższego ratingu. `contactyes` oznacza że kontakt z korkiem zwiększa prawdopodobieństwo wyższego ratingu. Progi rosną monotonicznie od -1.344 do 5.006, przy czym duże odstępy między progami 3|4 i 4|5 wskazują że przejście do wyższych kategorii jest trudniejsze.

Przekształciłem ratingu na zmienną binarną (wartości < 4 jako 0, wartości >= 4 jako 1) i stworzyłem dwie regresje. Współczynniki przy predyktorach są identyczne:

- `tempwarm`: 3.0312 w obu modelach
- `contactyes`: 1.8102 w obu modelach

Różnica pojawia się tylko w intercepcie - CLM zapisuje go jako próg `0|1 = 4.072`, regresja logistyczna jako `(Intercept) = -4.072`. Odwrotny znak wynika z tego że modele pracują w przeciwnych kierunkach: CLM modeluje P(Y ≤ kategorii), a regresja logistyczna P(Y = 1).

Gdy zmienna zależna ma tylko dwie kategorie, oba modele są matematycznie równoważne. CLM ma przewagę gdy kategorii jest więcej niż dwie - wtedy regresja logistyczna nie wystarczy.

```r
wine$rating_binary <- ifelse(as.numeric(wine$rating) < 4, 0, 1)

table(wine$rating_binary)

wine$rating_binary_factor <- factor(wine$rating_binary, ordered = TRUE)

model_clm_binary <- clm(rating_binary_factor ~ temp + contact, data = wine)
summary(model_clm_binary)

model_logit <- glm(rating_binary ~ temp + contact, data = wine, family = binomial)
summary(model_logit)

cat("CLM coefficients:\n")
coef(model_clm_binary)

cat("\nLogistic coefficients:\n")
coef(model_logit)
```

```r
> summary(model_clm_binary)
formula: rating_binary_factor ~ temp + contact
data:    wine

 link  threshold nobs logLik AIC   niter max.grad cond.H
 logit flexible  72   -28.73 63.46 6(0)  3.21e-12 3.1e+01

Coefficients:
           Estimate Std. Error z value Pr(>|z|)
tempwarm     3.0312     0.8525   3.556 0.000377 ***
contactyes   1.8102     0.6927   2.613 0.008964 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Threshold coefficients:
    Estimate Std. Error z value
0|1   4.0719     0.9393   4.335
>
> model_logit <- glm(rating_binary ~ temp + contact, data = wine, family = binomial)
> summary(model_logit)

Call:
glm(formula = rating_binary ~ temp + contact, family = binomial,
    data = wine)

Coefficients:
            Estimate Std. Error z value Pr(>|z|)
(Intercept)  -4.0719     0.9392  -4.335 1.45e-05 ***
tempwarm      3.0312     0.8525   3.556 0.000377 ***
contactyes    1.8102     0.6927   2.613 0.008963 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 83.100  on 71  degrees of freedom
Residual deviance: 57.457  on 69  degrees of freedom
AIC: 63.457

Number of Fisher Scoring iterations: 5

>
> cat("CLM coefficients:\n")
CLM coefficients:
> coef(model_clm_binary)
       0|1   tempwarm contactyes
  4.071873   3.031189   1.810247
>
> cat("\nLogistic coefficients:\n")

Logistic coefficients:
> coef(model_logit)
(Intercept)    tempwarm  contactyes
  -4.071873    3.031189    1.810247
```
