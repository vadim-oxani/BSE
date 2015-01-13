#########################################################################################
# The function is downloading the BSE stock prices from WSJ or kmarket.ro and reading   #
# the data into a data.table object.                                                    #
#########################################################################################
getBSEquotes <- function(symbol, start_date, end_date, source = 'wsj'){
                time_interval = as.Date(end_date) - as.Date(start_date)
				if(source == 'wsj'){		
						URL <- paste('http://quotes.wsj.com/RO/', symbol, 
						'/historical-prices/download?MOD_VIEW=page&num_rows=', 
						time_interval, '.0416666666667', '&range_days=', 
						time_interval,'.0416666666667', '&startDate=', 
						format(as.Date(start_date), '%m/%d/%y'), '&endDate=',
						format(as.Date(end_date), '%m/%d/%y'), sep = '')
						}
			    # The 'kmarket' source contains OHLC prices with thousands after the 
				# decimals point			
				else if(source == 'kmarket'){
						URL <- paste('http://www.kmarket.ro/inc/istoric.php?simbol=', 
						symbol, sep = '')
						}
				# Downloading data		
				library(data.table)
				if(!file.exists('data')){
				        dir.create('data')
				}
				setwd('./data')
				destfile = paste(symbol, '.csv', sep = '')
				download.file(URL, destfile, mode = 'wb')
			    # Reading data
				data <- fread(destfile)
				setwd('../')
				unlink('data', recursive = TRUE)
				# Cleaning data
				if(source == 'wsj'){
						setnames(data, c('Date', ' Open', ' High', ' Low', ' Close',
						' Volume'), c('DateTime', 'OPEN', 'HIGH', 'LOW', 'CLOSE',
						'VOLUME'))
						# Formatting DateTime 
						data[ ,DateTime:=as.POSIXct(DateTime, format = '%m/%d/%y', 
						tz = 'Europe/Bucharest')]
						}
			    else if(source == 'kmarket'){
						setnames(data, c('data', 'pret inchidere', 
						'pret inchidere ajustat', 'volum'), 
						c('DateTime', 'CLOSE', 'CLOSE_ADJUSTED',
						'VOLUME'))
						# Formatting DateTime 
						data[ ,DateTime:=as.POSIXct(DateTime, format = '%Y-%m-%d', 
						tz = 'Europe/Bucharest')]
						data <- data[DateTime >= as.POSIXct(start_date) & DateTime <= 
						as.POSIXct(end_date),]
						}
				setkey(data, DateTime)
				return(data)
}