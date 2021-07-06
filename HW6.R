######## q6
library(openintro)
data("starbucks")

#### a)
ggplot(data = starbucks) + 
  geom_point(aes(x = calories, y = carb), size = 3) + 
  labs(x = "Calories",  y = "Carbohyrdates (g)",
       title = "Calories and Grams of Carbs in 77 Starbucks Foods")
+ theme(plot.title = element_text(size = rel(.9)))

#### c)
model = lm(carb ~ calories, data = starbucks)

#### e)
summary(model)

#### f)
residual <- fortify(model)

ggplot(data = residual) + 
  geom_hline(yintercept = 0, size = 1.5) + 
  geom_point(aes(x = calories, y = .resid), size = 3) + 
  labs(x = "Calories", y = "Residual (g of carbs)", title = "Residual plot: modeling carbs with calories") + 
  theme(plot.title = element_text(size = rel(.9)))
