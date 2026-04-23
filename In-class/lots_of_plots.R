# notice that you have a bunch of red circles with white x's in them along the 
# lefthand side of the script. take note of what happens to these as we work  
# through this code together.

# what code will you need to add up here before you run the code below? why?

# plot 1
ggplot(penguins)

# plot 2
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g))

# plot 3
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) 

# plot 4
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

# plot 5
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

# plot 6
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

# plot 7
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

# plot 8
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point()

# plot 9
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(color = species)

# plot 10
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species))

# plot 11
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(fill = species))

# plot 12
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_histogram(aes(fill = species))

# plot 13
ggplot(penguins, aes(x = flipper_length_mm)) +
  geom_histogram(aes(color = species))

# plot 14
ggplot(penguins, aes(x = flipper_length_mm)) +
  geom_histogram(aes(fill = species))

# plot 15
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(color = "black", aes(shape = species))

# plot 16
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(color = species, aes(shape = species))

# plot 17
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(shape = species, color = species))

# plot 18
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point() +
  geom_smooth(method = "lm")

# plot 19
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species)) +
  geom_smooth(method = "lm")

# plot 20:
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm")
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  )

# plot 21:
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species, shape = species))
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  )
