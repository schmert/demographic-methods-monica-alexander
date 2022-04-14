124 -> k

this_pop = bigWPP$region[k]
this_NRR = bigNRR[k]
this_r   = bigR[k]
this_TFR = bigTFR[k]

txt = paste( 'TFR=', sprintf('%3.2f', this_TFR), ' r=',
                     sprintf('%4.3f', this_r))         

for (y in 1:38) { 
  plot(seq(0,100,5), bigProj[k,,y], type='h', lwd=5, 
       col='orangered', main=paste(this_pop, 2010+5*y),
       sub=txt,
       ylim=range(0,bigProj[k,,]))
}

