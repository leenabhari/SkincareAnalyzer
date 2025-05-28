library(tidyverse)

skincare <- read.csv("skincare_products_clean.csv")


# --- most common ingredients analysis ---

# clean and split the ingredients
ingredients_clean <- skincare %>%
  filter(!is.na(clean_ingreds)) %>%
  mutate(ingredient_list = clean_ingreds %>%
           str_remove_all("\\[|\\]|'") %>%  # remove brackets and single quotes
           str_split(",\\s*")) %>%          # split into list
  unnest(ingredient_list) %>%
  mutate(ingredient_list = str_to_lower(str_trim(ingredient_list))) %>% 
  count(ingredient_list, sort = TRUE)


# --- visualization of top 10 ingredients ---
top_10_ingredients <- ingredients_clean %>% slice_max(n, n = 10)

ggplot(top_10_ingredients, aes(x = reorder(ingredient_list, n), y = n)) +
  geom_col(fill = "#f9c5d1") +
  coord_flip() +
  labs(title = "Top 10 Ingredients in Skincare Products",
       x = "Ingredient", y = "Count") +
  theme_minimal()
