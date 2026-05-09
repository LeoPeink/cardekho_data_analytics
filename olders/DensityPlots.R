## Plotto la funzione di densita della variabile prezzo condizionata al carburante
plot_price_fuel <- function(k=1){
  values <- c();
  m_height <- c();
  m_width  <- c();
  for( f in unique(df$fuel) ){
    value <- density(df$selling_price[df$fuel==f]);
    values <- c(values, value);
    m_height <- c(m_height, max(value$y));
    m_width <- c(m_width, max(value$x));
  }
  m_height <- max(m_height);
  m_width <- max(m_width);

  plot(x=NULL, y=NULL, xlim=range(0,m_width/k), ylim=range(0,m_height), ylab="density(selling_price)", xlab="fuel");
  lines(density(df$selling_price[df$fuel=="Petrol"]), col="#008507");
  lines(density(df$selling_price[df$fuel=="Diesel"]), col="red");
  lines(density(df$selling_price[df$fuel=="LPG"]), col="blue");
  lines(density(df$selling_price[df$fuel=="CNG"]), col="orange");
  legend("topright", legend=c("LPG","CNG","Petrol","Diesel"), pch=16, col=c("blue","orange","#008507","red") );
}

plot_price_fuel(cleanedCSV, k=5)

# plot(ecdf(max))
# to_trim = 1/length(max[!is.na(max)])
# media <- mean(max, trim=to_trim, na.rm=T)
# sd <- sd_trim(max[!is.na(max)], trim=to_trim)
# curve(pnorm(x,media,sd), add=T, col="red")
# 
# 
# plot(ecdf(rescale(df$km_driven)))




# ecdf_comparison <- function(variabile){
  # max_new <- variabile
  # plot(ecdf(max_new))
  # to_trim = 1/length(max_new[!is.na(max_new)])
  # media <- mean(max_new, na.rm=T)
  # sd <- sd_trim(max_new[!is.na(max)])
  # curve(pnorm(x,media,sd), add=T, col="red")
}


# how to read qqplot
# https://stats.stackexchange.com/questions/101274/how-to-interpret-a-qq-plot
