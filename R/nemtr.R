nemtr <- function(dataFrame, timing, streams, VoI = NA, type="long", median0 = NA, delta = 3){
  dataProc <- dataRead(dataFrame, timing, streams, VoI, type, median0, delta)
  dataProc  %>% group_by(streams) %>% summarise(num.streams = n()) -> c
  c <- length(c$num.streams)
  dataProc %>%  group_by(timing) %>% summarise(num.streams = n()) -> t
  t <- length(t$num.streams)
  n <- length(unique(dataProc$streams))
  dataProc %>%
    filter(VoI > median0) %>%
      group_by(streams,timing) %>%
        summarise(frequency = n()) %>%
          mutate(zt = (frequency - (n*0.50))/((n*0.25)**0.50))  -> nemt_cusum

  nemt_cusum %>%
    group_by(timing) %>%
    summarise(EMT = sum(zt)) %>%
    mutate(st = cumsum(EMT),
           stLag = lag(st),
           UL = ifelse(is.na(stLag), delta*(c**0.5), stLag + (delta*(c**0.5))),
           LL = ifelse(is.na(stLag), -1*delta*(c**0.5), (2*stLag) - UL),
           OOC = ifelse(st > UL | st < LL, timing,NA)) -> nemt_plot

  ggplot(data = nemt_plot, aes(x = timing)) +
    geom_line(aes(y = st)) +
    geom_line(aes(y = UL)) +
    geom_line(aes(y = LL)) +
    geom_point(aes(y = st))

}

#nemtr(df, timing="hour", streams=c("rep1", "rep2", "rep3", "rep4", "rep5", "rep6", "rep7", "rep8", "rep9", "rep10"), type="wide", median0 = .8)



#median0 = 0.8
#n <- length(unique(expOutput$streams))
#expOutput %>%
#  filter(VoI > median0) %>%
#  group_by(streams,timing) %>%
#  summarise(frequency = n()) %>%
#  mutate(zt = (frequency - (n*0.50)/(n*2.5)**0.50))  -> nemt_cusum

#turnstiles %>% group_by(turnstile,hour) %>% summarise(num.streams = n()) -> e
#expOutput  %>% group_by(streams) %>% summarise(num.streams = n()) -> c

#n <- e[1,3]
