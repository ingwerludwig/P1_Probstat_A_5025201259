# NAMA  : INGWER LUDWIG NOMMENSEN
# NRP   : 5025201259
# KELAS : PROBSTAT A

# 1
  #a
    p = 0.20
    n = 3
    #dgeom(x, prob) = dgeom: returns the value of the geometric probability
    dgeom(x <- n, prob <- p)

  #b
    #rgeom(n, prob) = generates a vector of geometric distributed random variables (distribusi geometrik acak)
    mean(rgeom(n <- 10000, prob <- p) == 3)

  #c
    #Kesimpulan : Pada soal 1a itu merupakan nilai exact nya, yaitu 0.1024 sedangkan 1.b merupakan nilai dari simulasi ketika diambil dengan 10000 data random pada kasus yang serupa dan ternyata hasil nya mendekati nilai exact nya

  #d
    library(dplyr)
    library(ggplot2)
    
    data.frame(x = 0:20, prob <= dgeom(x = 0:20, prob = p) ) %>%
      
      #populate 'other' if x!=n)
      mutate(Failures = ifelse(x == n, n, "other")) %>%
      
      #visualtization 
      ggplot(aes(x = factor(x), y = prob, fill = Failures)) +
      geom_col() +
      geom_text(
        aes(label = round(prob,2), y = prob + 0.01),
        position = position_dodge(0.9),
        size = 3,
        vjust = 0
      ) +
      labs(title = "Histogram Distribusi Geometrik , Peluang X = 3 gagal Sebelum Sukses Pertama",
           subtitle ="",
           x = "(x)",
           y = "Probability"
      )
    print(data.frame)

  #e
    p=0.2
    #Nilai Rataan (μ)
      mean1 = 1 / p
      mean1
    #Varian (σ2)
      var1 = (1 - p) / p^2
      var1

    
# 2
  # Diketahui :
  # jumlah pasien = n = 20
  # peluang sembuh = p = 0.2
  
  #a
    # dbinom(x, size, prob)
    a <- dbinom(x=4,20,0.2)
    a
  
  
  #b
    data <- rbinom(20,20,0.2)
    hist(data,
         xlim = c(0,8))
  
  #c
    n=20 #jumlah pasien
    p=0.2 #peluang
    q=1-p #peluang failure nya (distirbusi binomial)
  
  #Nilai Rataan (μ) 
    me=n*p
    me
  
  #Nilai Rataan (μ) sample
    mean(data)
  
  #Varian (σ2) 
    va=n*p*q
    va
  
  #Varian (σ2)  sample
    var(data)


# 3
    
  #a
    x = 6
    lambda = 4.5
    dpois(x,lambda)
  
  #b
    set.seed(2)
  
    babies <- data.frame('data' = rpois(365, 4.5))
  
    babies %>% ggplot() +
      geom_histogram(aes(x = data,
                         y = stat(count / sum(count)),
                         fill = data == 6),
                     binwidth = 1,
                     color = 'black',) +
      scale_x_continuous(breaks = 0:10) + 
      labs(x = 'Jumlah bayi yang lahir per periode',
           y = 'Proporsi',
           title = '365 simulasi kelahiran di rumah sakit dengan Pois(lambda = 4.5)') +
      theme_bw()
  
  #c
    babies %>% dplyr::summarize(six_babies = sum(babies$data == 6) / n())
    #dapat dilihat bahwa hasil simulasi nya sekitar 11.5% (berdasarkan perhitungan diatas)
    #sedangkan nilai exact nya yaitu 12,8%
    #yang artinya sesuai karena nilai simulasi mendekati nilai exact
    
  #d
    #mean
    lambda
  
  #variance
    lambda



# 4

  #a
    x = 2
    df = 10
    dchisq(x , df)
  
  #b
    p <- rchisq(100,df)
  
    hist(p,
         freq = FALSE,
         xlim = c(0,30),
         ylim = c(0,0.2),
         main = "Histogram dari Distribusi Chi-Square")
  
  #c
    #mean
    mean1 = df
    mean1
  
  #variance
    var1 = 2*df
    var1



# 5

  #a
    lambda = 3
    #jika bilangan random sejumlah n=10
    rexp(10,rate = lambda)
  
  #b
    #n = 10
    set.seed(1)
    x1 <- rexp(10,rate = lambda)
    hist(x1,
         main = "Histogram Exponesial if n = 10")
    #n = 100
    set.seed(1)
    x2 <- rexp(100,rate = lambda)
    hist(x2,
         ylim = c(0,50),
         xlim = c(0,2),
         main = "Histogram dari Distribusi Exponensial jika n = 100")
    #n=1000
    set.seed(1)
    x3 <- rexp(1000,rate = lambda)
    hist(x3,
         ylim = c(0,500),
         xlim = c(0,3),
         main = "Histogram dari Distribusi Exponensial jika n = 1000")
    #n=10000
    set.seed(1)
    x4 <- rexp(10000,rate = lambda)
    hist(x4,
         ylim = c(0,5000),
         xlim = c(0,4),
         main = "Histogram dari Distribusi Exponensial jika n = 10000")
  
  #c
    n = 100
    lamd = 3
    data <- rexp(n, rate = lamd)
    avg <- mean(data)
    avg
    va <- var(data)
    va



# 6

  #a
    n = 100
    m = 50
    std = 8
  
  #data <- c(1,2,4,2,6,3,10,11,5,3,6,85)
  data <- rnorm(100,50,8)
  rata_rata <- mean(data)
  x1 <- floor(mean(data))
  x2 <- round(mean(data))
  
  z_scores <- (data - mean(data) / sd(data))
  
  plot(z_scores, type = "o", col="red")
  
  #b
    x <- rnorm(100,50,8)
    hist(x,
         breaks = 50,
         main = "Grafik Histogram",col = 'blue' )
  
  #c
    var(x)

