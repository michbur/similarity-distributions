library(GOSemSim)
library(ggplot2)
library(pbapply)

hsGO <- godata('org.Hs.eg.db', ont="MF")

finite_go <- names(slot(hsGO, "IC")[!is.infinite(slot(hsGO, "IC"))])

set.seed(15922)

sims <- do.call(rbind, pblapply(1L:10000, function(dummy) {
  x1 <- sample(x = finite_go, size = sample(1L:6, 1), replace = FALSE)
  x2 <- sample(x = finite_go, size = sample(1L:6, 1), replace = FALSE)
  
  data.frame(
    x1 = paste0(x1, collapse = ";"),
    x2 = paste0(x2, collapse = ";"),
    sim = mgoSim(x1, x2, semData = hsGO, measure = "Rel", combine = "BMA"))
}))

png("far.png")
ggplot(sims, aes(x = sim)) +
  geom_density() +
  #geom_vline(aes(xintercept = median(sim)), color = "green") +
  geom_vline(aes(xintercept = mean(sim)), color = "blue") +
  scale_x_continuous("Similarity") +
  theme_bw()
dev.off()


sims_close <- do.call(rbind, pblapply(1L:10000, function(dummy) {
  x1 <- sample(x = finite_go, size = sample(1L:6, 1), replace = FALSE)
  x2 <- c(x1, sample(x = setdiff(finite_go, x1), size = sample(1L:3, 1), replace = FALSE))
  
  data.frame(
    x1 = paste0(x1, collapse = ";"),
    x2 = paste0(x2, collapse = ";"),
    sim = mgoSim(x1, x2, semData = hsGO, measure = "Rel", combine = "BMA"))
}))

png("close.png")
ggplot(sims_close, aes(x = sim)) +
  geom_density() +
  #geom_vline(aes(xintercept = median(sim, na.rm = TRUE)), color = "green") +
  geom_vline(aes(xintercept = mean(sim, na.rm = TRUE)), color = "blue") +
  scale_x_continuous("Similarity")+
  theme_bw()
dev.off()

save(sims, sims_close, file = "sims.RData")
