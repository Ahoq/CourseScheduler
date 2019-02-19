calculate_result<-function(){
  
  Transportation <- 5
  Schools <- 5
  Medical <- 4
  Crime <- 1
  Income <- 3
  Bars_and_Restaurants <- 5
  Park <- 5
  Recreation <- 5
  Store <- 5
  
  
  final_score = 2 * (Transportation + Schools + Medical - Crime + Income) + Bars_and_Restaurants+
    Park + Recreation + Store
  median_sales_price <-as.integer(readline(prompt = "PLEASE ENTER THE MEDIAN SALES PRICE: "))
  
  
  data_set <- data.frame(Transportation, Schools , Medical , Crime , Income , Bars_and_Restaurants,
    Park , Recreation , Store)
  
  m <- melt(data_set)
  
  label = c('Transportation', 'School' , 'Medical Facilities' , 'Crime' , 'Income' ,
            'Bars & Restaurants',
            'Parks' , 'Recreations' , 'Stores')
  
  result <- round((final_score / median_sales_price),5)
  f_result <- paste("Result: ", result)
  print(final_score)
  print(f_result)
  
  ggplot(m, aes(variable, value,  fill = variable)) + 
    labs(fill="",y="",x="", title = paste("Result: ", result)
    )+ theme_classic()+ 
    geom_bar(stat="identity")+
    geom_abline(aes(slope=0, intercept=0))+
    scale_colour_manual(values=c("black"))+
    geom_text(aes(label=value),position=position_stack(vjust=1.05),fontface="bold")+
    scale_y_continuous(limits=c(0,6),breaks=seq(0,6, by=1))+
    theme(legend.text=element_text(size=12,face = "bold"))+
    theme(axis.text.x = element_text(size = 10,face = "bold"))+
    theme(axis.text.y = element_text(size = 10,face = "bold"))+
    theme(plot.title = element_text(color = "#af0808",face = "bold"))
    #scale_fill_manual(breaks = c('Transportation', 'School' , 'Medical Facilities' , 'Crime' , 'Income' ,
                                # 'Bars & Restaurants',
                                 #'Parks' , 'Recreations' , 'Stores'))+
    #scale_x_discrete(labels = label, breaks = m$variable)
  
}

Trans <-function(){
  subways <- as.integer(readline(prompt = "PLEASE ENTER THE NUMBER OF SUBWAYS: "))
  buses <- as.integer(readline(prompt = "PLEASE ENTER THE NUMBER OF BUSES: "))
  trains <- as.integer(readline(prompt = "PLEASE ENTER THE NUMBER OF TRAINS: "))
  transportation <- 3 * subways + 2 * trains + buses 
  if (transportation >= 25){
    t_score <- 5
    return (t_score)
  }else if (20 <= transportation && transportation < 25){
    t_score <- 4
    return (t_score)
  }else if (15 <= transportation && transportation < 20){
    t_score <- 3
    return (t_score)
  }else if (10 <= transportation && transportation < 15){
    t_score <- 2
    return (t_score)
  }else if (transportation < 10){
    t_score <- 1
    return (t_score)
  }
}

Schools <-function(myfile){
  ds <- read_excel(myfile, sheet = 1)
  num_1 <- 1* ds$num_of_schools_with_rating_1
  num_2 <- 2* ds$num_of_schools_with_rating_2
  num_3 <- 3*ds$num_of_schools_with_rating_3
  num_4 <- 4*ds$num_of_schools_with_rating_4
  num_5 <- 5*ds$num_of_schools_with_rating_5
  num_6 <- 6*ds$num_of_schools_with_rating_6
  num_7 <- 7*ds$num_of_schools_with_rating_7
  num_8 <- 8*ds$num_of_schools_with_rating_8
  num_9 <- 9*ds$num_of_schools_with_rating_9
  num_10 <- 10*ds$num_of_schools_with_rating_10
  
  school <- num_1+num_2+num_3+num_4+num_5+num_6+num_7+num_8+num_9+num_10
  college <- as.integer(readline(prompt = "PLEASE ENTER THE NUMBER OF COLLEGES/UNIVERSITIES: "))
  
  schools <- school + (7*college)
  
  if (schools >= 525){
    print(schools)
    s_score <- 5
    return (s_score)
  }else if (425 <= schools && schools < 525){
    s_score <- 4
    return (s_score)
  }else if (325 <= schools && schools < 425){
    s_score <- 3
    return (s_score)
  }else if (225 <= schools && schools < 325){
    s_score <- 2
    return (s_score)
  }else if (schools < 225){
    s_score <- 1
    return (s_score)
  }
  
}

Medical <- function(){
  hospitals <- as.integer(readline(prompt = "PLEASE ENTER THE NUMBER OF HOSPITALS: "))
  others <- as.integer(readline(prompt = "PLEASE ENTER THE NUMBER OF OTHER MEDICAL FACILITIES: "))
  medical <- 2 * hospitals + others
  if (medical >= 140){
    md_score <- 5
    return (md_score)
  }else if (120 <= medical && medical < 140){
    md_score <- 4
    return (md_score)
  }else if (100 <= medical && medical < 120){
    md_score <- 3
    return (md_score)
  }else if (80 <= medical && medical < 100){
    md_score <- 2
    return (md_score)
  }else if (medical < 80){
    md_score <- 1
    return (md_score)
  }
}

Income <- function(){
  income <- as.integer(readline(prompt = "PLEASE ENTER THE MEDIAN INCOME: "))

  if (income >= 80){
    i_score <- 5
    return (i_score)
  }else if (70 <= income && income < 80){
    i_score <- 4
    return (i_score)
  }else if (60 <= income && income < 70){
    i_score <- 3
    return (i_score)
  }else if (50 <= income && income < 60){
    i_score <- 2
    return (i_score)
  }else if (income < 50){
    i_score <- 1
    return (i_score)
  }
}
Crime <- function(){
  crime <- as.double(readline(prompt = "PLEASE ENTER THE RATIO OF CRIME PER THOUSAND PEOPLE: "))
  
  if (crime >= 7.1){
    c_score <- 5
    return (c_score)
  }else if (5.5 <= crime && crime < 7.1){
    c_score <- 4
    return (c_score)
  }else if (3.9 <= crime && crime < 5.5){
    c_score <- 3
    return (c_score)
  }else if (2.3 <= crime && crime < 3.9){
    c_score <- 2
    return (c_score)
  }else if (0.7 <= crime && crime < 2.3){
    c_score <- 1
    return (c_score)
  }
}

Park <-function(myfile){
  ds <- read_excel(myfile, sheet = 2)
  parks <- sum(ds$score)
  
  if (parks >= 170){
    p_score <- 5
    return (p_score)
  }else if (130 <= parks && parks < 170){
    p_score <- 4
    return (p_score)
  }else if (90 <= parks && parks < 130){
    p_score <- 3
    return (p_score)
  }else if (50 <= parks && parks < 90){
    p_score <- 2
    return (p_score)
  }else if (parks < 50){
    p_score <- 1
    return (p_score)
  }
}
Bars_and_Restaurants <- function(){
  restaurants_w_bars <- as.integer(readline(prompt = "PLEASE ENTER THE NUMBER OF RESTAURANTS WITH BARS: "))
  restaurants_w_o_bars <- as.integer(readline(prompt = "PLEASE ENTER THE NUMBER OF RESTAURANTS WITHOUT BARS: "))
  bars <- as.integer(readline(prompt = "PLEASE ENTER THE NUMBER OF BARS: "))
  deli <- as.integer(readline(prompt = "PLEASE ENTER THE NUMBER OF DELIS: "))
  coffee_shops <- as.integer(readline(prompt = "PLEASE ENTER THE NUMBER OF COFFEE SHOPS: "))
  
  res_and_bars <- (4 * restaurants_w_bars) + (3 * restaurants_w_o_bars) + (2 * bars) + (1 * deli) + (1 * coffee_shops)
  if (res_and_bars >= 750){
    res_score <- 5
    print(res_and_bars)
    return (res_score)
  }else if (650 <= res_and_bars && res_and_bars < 750){
    res_score <- 4
    print(res_and_bars)
    return (res_score)
  }else if (550 <= res_and_bars && res_and_bars < 650){
    res_score <- 3
    print(res_and_bars)
    return (res_score)
  }else if (450 <= res_and_bars && res_and_bars < 550){
    res_score <- 2
    print(res_and_bars)
    return (res_score)
  }else if (res_and_bars < 450){
    res_score <- 1
    print(res_and_bars)
    return (res_score)
  }
}

Recreation <- function(){
  recreations <- as.integer(readline(prompt = "PLEASE ENTER THE RECREATIONAL FACILITIES: "))
  
  if (recreations >= 140){
    rec_score <- 5
    return (rec_score)
  }else if ( 120<= recreations && recreations < 140){
    rec_score <- 4
    return (rec_score)
  }else if (100 <= recreations && recreations < 120){
    rec_score <- 3
    return (rec_score)
  }else if (80 <= recreations && recreations < 100){
    rec_score <- 2
    return (rec_score)
  }else if (recreations < 80){
    rec_score <- 1
    return (rec_score)
  }
}

Store <- function(){
  supermarkets <- as.integer(readline(prompt = "PLEASE ENTER THE NUMBER OF SUPERMARKETS: "))
  others <- as.integer(readline(prompt = "PLEASE ENTER NUMBER OF OTHER STORES: "))
  stores <- 1.5 * supermarkets + others
  if (stores >= 650){
    print(stores)
    store_score <- 5
    return (store_score)
  }else if (575 <= stores && stores < 650){
    store_score <- 4
    return (store_score)
  }else if (500 <= stores && stores < 575){
    store_score <- 3
    return (store_score)
  }else if (425 <= stores && stores < 500){
    store_score <- 2
    return (store_score)
  }else if (stores < 425){
    store_score <- 1
    return (store_score)
  }
}