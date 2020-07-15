# Load library.
library(conflicted)

# Resolve conflicts.
conflict_prefer('mutate', 'dplyr')
conflict_prefer('select', 'dplyr')
conflict_prefer('arrange', 'dplyr')
conflict_prefer('filter', 'dplyr')
conflict_prefer('summarise', 'dplyr')
conflict_prefer('summarize', 'dplyr')
conflict_prefer('n', 'dplyr')
conflict_prefer('rename', 'dplyr')
conflict_prefer('distinct', 'dplyr')
