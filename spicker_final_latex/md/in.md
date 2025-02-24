# Wichtige Befehle
## Librarys
```R
library(tidyverse)
library(TeachingDemos)
```

## Datenerzeugung
- **sample** = Erzeugt ein sample aus den Werten in dem Array x mit der Länge size. Replace auf True wenn wir nur Unique werte aus x wollen.
- **runif** = Ein array mit länge n mit werten von min bis max
```R
students <- tibble(
  id = 1:10,
  sex = sample(x = c("f", "m"), size = 10, replace = T),
  age = round(runif(n = 20, min = 10, max = 35)),
)
```
