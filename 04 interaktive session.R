par(mfrow=c(2,2))          # Teilt das Ausgabefenster in 4 Bereiche
attach(SalesOrders)        # für Schreibfaule, nie in Scripten verwenden!
names(SalesOrders)         # Spaltennamen auslesen
hist(BuyerRef)             # Histogram für erste Spalte
hist(DeliveryDate, "weeks")# spezielle hist() Fkt für Date Klasse
hist(TotalPrice)           # ….
hist(PaymentDuration)      # ….

# 2-dimensionale Korrelationsplots
plot(BuyerRef, PaymentDuration)
smoothScatter(BuyerRef, PaymentDuration)
smoothScatter(TotalPrice, PaymentDuration)
plot(TotalPrice, PaymentDuration, cex=0.3)


# not part of Heise article
detach("SalesOrders")

scatterplotMatrix(SalesOrders, diagonal="histogram", cex=0.5, col="blue")
