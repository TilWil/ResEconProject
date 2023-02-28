################################################################################
### Marginal Willingness to Pay (MWTP)

WTP<- wtp(cost ="b_price", attr = "b_FoBio",modelname = model3)
allwtp <-wtp(cost="b_price", attr = names(model3$estimate), modelname = model3)

# modeltex <- quicktexregapollo(model3,wtpest = allwtp)
# screenreg(modeltex, digits=4)
# texreg(modeltex)

### End
################################################################################