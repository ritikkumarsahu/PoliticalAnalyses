#compare positive and negative sentiments of both parties
c(party = 'BJP', sentiment_score_BJP[c('positive', 'negative')], party = 'Congress', sentiment_score_Congress[c('positive', 'negative')])
BJP_Ratio = sentiment_score_BJP['positive'] / sentiment_score_BJP['negative']
Congress_Ratio = sentiment_score_Congress['positive'] / sentiment_score_Congress['negative']

if (BJP_Ratio > Congress_Ratio){ XLAB = 'Bhartiya Janta Party will Win the 2019 Election!' } else { XLAB = 'Congress Party will Win the 2019 Election!' }
    barplot(c(BJP_Ratio,Congress_Ratio),
        las = 2,
        col = rainbow(2),
        xlab = XLAB,
        main = 'Result Graph',
        names.arg = c("BJP", "Congress"),
        horiz = TRUE)
