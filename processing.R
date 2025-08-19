
# Perparation
library(tidyverse)
library(openxlsx)

####################################################################
## Initial processing of the ILO data 
####################################################################

structure <- read.xlsx("Input//ISCO-08 EN Structure and definitions.xlsx") %>%
  janitor::clean_names () 


definition_l1 <- structure %>%
  # filter only level-2 for taxonomies
  filter (level == "1") %>%
  mutate(
    tasks_include = str_remove(tasks_include, paste0("Tasks performed by ", str_to_lower(title_en)," usually include: \\s*")),
    task_list = str_split(tasks_include, ";\\s*")
  ) %>%
  select(isco_08_code, task_list) %>%
  unnest(task_list) %>%
  mutate(
    task = str_trim(task_list),
    task = str_remove(task, "\\.$"),  # Remove final period
    task = str_to_sentence(task)
  ) %>%
  select(isco_08_code, task)

definition_l2 <- structure %>%
  # filter only level-2 for taxonomies
  filter (level == "2") %>%
  select (isco_08_code, tasks_include) %>%
  mutate(
    tasks_include = str_remove(tasks_include, "Tasks performed by workers in this sub-major group usually include: \\s*"),
    task_list = str_split(tasks_include, ";\\s*")
  ) %>%
  select(isco_08_code, task_list) %>%
  unnest(task_list) %>%
  mutate(
    task = str_trim(task_list),
    task = str_remove(task, "\\.$"),  # Remove final period
    task = str_to_sentence(task)
  ) %>%
  select(isco_08_code, task)

definition_l3 <- structure %>%
  # filter only level-3 for taxonomies
  filter (level == "3") %>%
  select (isco_08_code, tasks_include) %>%
  mutate(
    tasks_include = str_remove(tasks_include, "Tasks performed usually include:\\s*"),
    task_list = str_split(tasks_include, ";\\s*")
  ) %>%
  select(isco_08_code, task_list) %>%
  unnest(task_list) %>%
  mutate(
    task = str_trim(task_list),
    task = str_remove(task, "\\.$"),  # Remove final period
    task = str_to_sentence(task)
  ) %>%
  select(isco_08_code, task)

definition_l4 <- structure %>%
  # filter only level-4 for taxonomies
  filter (level == "4") %>%
  select (isco_08_code, tasks_include) %>%
  mutate(
    tasks_include = str_remove(tasks_include, "Tasks include -\\s*"),  # Remove lead-in
    task_list = str_extract_all(tasks_include, "\\(\\w\\)\\s[^\\(\\)]+(?:;|\\.)")  # Extract bullet points
  ) %>%
  select(isco_08_code, task_list) %>%
  unnest(task_list) %>%
  mutate(
    task = str_remove(task_list, "^\\(\\w\\)\\s*"),  # Remove (a), (b), etc.
    task = str_trim(task),
    task = str_remove(task, ";$|\\.$"),  # Remove trailing punctuation
    task = str_to_sentence(task)
  ) %>%
  select(isco_08_code, task) 

definition <- definition_l1 %>%
  rbind (definition_l2) %>%
  rbind (definition_l3) %>%
  rbind (definition_l4) %>%
  arrange (isco_08_code) %>% drop_na (task) 

task_for_clustering <- definition %>%
  distinct (task)

remove (definition_l1) 
remove (definition_l2) 
remove (definition_l3) 
remove (definition_l4) 


####################################################################
# Task to level-2 after clustering and labelling work done on Python
####################################################################

taxonomy_l2 <- read.xlsx ("content//task_clusters_labeled.xlsx") %>%
  # Somehow there is a single task being clustered multiple times. Avoid this situation by enforcing priority of classification methods.
  mutate (priority = case_when (
    methodology == "FirstPass_HDBSCAN" ~ 1,
    methodology == "SecondPass_HDBSCAN_ISCO" ~ 2,
    methodology == "Final_Agglomerative_ISCO" ~ 3  )) %>%
  group_by (task) %>%
  filter (priority == min(priority)) %>%
  ungroup () %>%
  select (-priority)

######################## Manual correction #########################

l2_label_forinspection <- taxonomy_l2 %>%
  select (cluster_label, task, label_fit_score, label_fit_explanation, everything()) %>%
  arrange (label_fit_score, cluster_label, task)

# Make a small table of corrections
# Key it by something stable (e.g., task + isco_08_code).
edits <- tibble::tribble(
  ~task,                          ~isco_08_code, ~cluster_label, ~methodology, ~cluster_id, ~label_source,
  
  "Washing and polishing floors", 91, "Washing and polishing floors", "Manual_Override", 1, "Manual",
  
  "Cleaning carpets and upholstered furniture using cleaning machines and their attachments;",       9129, "Vehicle and Upholstery Cleaning", "Manual_Override", 2, "Manual",
  "Cleaning, washing and polishing cars and other vehicles, by hand or using hand-held power tools", 9122, "Vehicle and Upholstery Cleaning", "Manual_Override", 2, "Manual",
  "Vacuuming vehicle interiors and dry-cleaning carpets and upholstery",                             9122, "Vehicle and Upholstery Cleaning", "Manual_Override", 2, "Manual",
 
  "Shovelling snow", 9613, "Shovelling snow", "Manual_Override", 3, "Manual",
  
  "Cleaning chimneys",                                       713,  "Chimney Cleaning Services", "Manual_Override", 4, "Manual",
  "Removing soot from flues, chimneys and connecting pipes", 7133, "Chimney Cleaning Services", "Manual_Override", 4, "Manual",
  
  "Removing asbestos, mould and fire-damaged surfaces from buildings", 7133, "Removing asbestos, mould and fire-damaged surfaces from buildings", "Manual_Override", 5, "Manual",
  
  "Performing amusing antics and telling funny stories", 2659, "Performing amusing antics and telling funny stories", "Manual_Override", 6, "Manual",
  
  "Performing tricks of illusion and sleight of hand, and feats of hypnotism", 2659, "Performing tricks of illusion and sleight of hand, and feats of hypnotism", "Manual_Override", 7, "Manual",
  
  "Fabricating full and partial dentures and constructing mouth guards, crowns, metal clasps, inlays, bridgework and other aids", 3214, "Fabricating full and partial dentures and constructing mouth guards, crowns, metal clasps, inlays, bridgework and other aids", "Manual_Override", 8, "Manual", 
  "Incising and closing incisions on various parts of the body and reshaping or reconstructing disfigured or maimed bodies when necessary", 5163, "Incising and closing incisions on various parts of the body and reshaping or reconstructing disfigured or maimed bodies when necessary", "Manual_Override", 9, "Manual", 
  "Making or receiving casts or impressions of patients' torsos, limbs, mouths or teeth for use as fabrication patterns", 3214, "Making or receiving casts or impressions of patients' torsos, limbs, mouths or teeth for use as fabrication patterns", "Manual_Override", 10, "Manual",

  "Determining how food should be presented, and creating decorative food displays", 3434, "Designing and arranging food presentations", "Manual_Override", 11, "Manual",
  "Monitoring quality of dishes at all stages of preparation and presentation",      3434, "Designing and arranging food presentations", "Manual_Override", 11, "Manual",
  
  "Constructing features and facilities within gardens, such as paths or paved areas, walls, rockeries, garden beds, ponds and water features, sheds and fences", 6113, "Garden and Land Enhancement", "Manual_Override", 12, "Manual", 
  "Preparing land by conditioning soil, levelling ground and installing and operating irrigation and drainage systems",                                           6113, "Garden and Land Enhancement", "Manual_Override", 12, "Manual", 
  "Investing in land and land improvements",                                                                                                                      611,  "Garden and Land Enhancement", "Manual_Override", 12, "Manual", 
  
  "Operating and monitoring machinery and equipment used to produce articles made of metal, minerals, chemicals, rubber, plastics, wood, paper, textiles, fur or leather, and which process foodstuffs and related products", 8, "Operating and monitoring machinery and equipment used to produce articles made of metal, minerals, chemicals, rubber, plastics, wood, paper, textiles, fur or leather, and which process foodstuffs and related products", "Manual_Override", 13, "Manual", 

  "Tending vacuum container which moistens tobacco for further processing", 7516, "Tending vacuum container which moistens tobacco for further processing", "Manual_Override", 14, "Manual", 
  "Transferring preserved foods to sterile jars, bottles or other containers", 7514, "Transferring preserved foods to sterile jars, bottles or other containers", "Manual_Override", 15, "Manual", 
  
  "Operating and tending kilns, treating tanks and other equipment to dry lumber, prepare and season wood and other wood products, and to impregnate wood products with preservatives", 7521, "Wood Processing and Treatment", "Manual_Override", 16, "Manual", 
  "Operating and tending treating and other equipment to dry lumber and other wood products, and to treat chemically and impregnate wood products with preservatives"                 , 752 , "Wood Processing and Treatment", "Manual_Override", 16, "Manual", 
  "Operating preset special-purpose woodworking machines to fabricate wooden products such as coat hangers, mop handles, clothes pins and other products"                             , 7523, "Wood Processing and Treatment", "Manual_Override", 16, "Manual", 
  
  "Operating smokehouses or ovens to smoke meat, fish and other foodstuffs", 7511, "Operating smokehouses or ovens to smoke meat, fish and other foodstuffs", "Manual_Override", 17, "Manual")

# 2) Apply in place; only the columns present in `edits` are updated
l2_label_forinspection <- l2_label_forinspection %>%
  # Manual updates
  rows_update(edits, by = c("task", "isco_08_code")) %>%
  # Mass updates
  mutate (cluster_label = case_when (
    cluster_label == "Designing and arranging decorative displays for food and exhibit presentations" ~ "Designing and arranging decorative displays exhibit presentations",
    TRUE ~ cluster_label
  ))

prefix <- c(
    "FirstPass_HDBSCAN"    = "CORE",
    "SecondPass_HDBSCAN"   = "SECONDARY_HDB",
    "Final_Agglomerative_ISCO" = "SECONDARY_AGG",
    "Manual_Override"      = "MANUAL")

taxonomy_l2 <- l2_label_forinspection %>%
  mutate(unified_cluster_id = paste0(dplyr::recode(methodology, !!!prefix, .default = "OTHER"),
                                     "_", cluster_id))

################### Level-1 identifier "l1_xxxx" ###################
taxonomy_l2_l1labelled <- taxonomy_l2 %>% 
  mutate(
    isco_08_code = as.character(isco_08_code),
    task = as.character(task)
  ) %>%
  left_join(
    taxonomy_l2 %>%
      mutate(
        isco_08_code = as.character(isco_08_code),
        task = as.character(task)
      ) %>%
      # Attach custom ISCO order (digit length, then lexicographic within length)
      left_join(
        taxonomy_l2 %>%
          mutate(isco_08_code = as.character(isco_08_code)) %>%
          distinct(isco_08_code) %>%
          mutate(n_digits = nchar(isco_08_code)) %>%
          arrange(n_digits, isco_08_code) %>%
          mutate(isco_order = row_number()) %>%
          select(isco_08_code, isco_order),
        by = "isco_08_code"
      ) %>%
      # For each task, find the earliest ISCO position and rank tasks accordingly
      group_by(task) %>%
      summarise(first_isco_order = min(isco_order, na.rm = TRUE), .groups = "drop") %>%
      mutate(first_isco_order = ifelse(is.finite(first_isco_order), first_isco_order, Inf)) %>%
      arrange(first_isco_order, task) %>%
      mutate(l1_identifier = paste0("l1_", str_pad(row_number(), 4, pad = "0"))) %>%
      select(task, l1_identifier),
    by = "task"
  ) 

taxonomy_l1_final <- taxonomy_l2_l1labelled %>%
  select (l1_identifier, "l1_label" = task, isco_08_code) %>%
  distinct () %>%
  arrange (l1_identifier)

l1_distinct <- n_distinct(taxonomy_l1_final$l1_identifier)

################### Level-2 identifier "l2_xxxx" ###################

# ─────────────────────────────────────────────────────────────────────────────
# Level-2 numbering and naming workflow with post-hoc Auto-prefix review + override
# Expected input dataframe: taxonomy_l2_l1labelled
# Required columns (min): unified_cluster_id, l1_identifier, task, methodology,
#                         label_source, cluster_label, label_fit_score
# Output objects:
#   1) taxonomy_l2_final         -> production table with requested columns
#   2) l2_auto_review_table      -> review table of clusters using Auto labels
#                                   (lists all tasks per cluster)
#   3) auto_review_overrides     -> EDIT HERE block to replace Auto labels
#   4) cluster_overrides         -> authoritative manual labels (wins always)
# ─────────────────────────────────────────────────────────────────────────────

library(purrr)
library(tibble)

# ── 0) Authoritative manual labels (highest precedence) ----------------------
# Keep this short and editable. Add rows as needed.
cluster_overrides <- tribble(
  ~unified_cluster_id,  ~manual_override_label,
  "CORE_110",  "Authorising material, human and financial resources to implement policies and programmes",
  "CORE_227",  "Preparing detailed estimates of quantities and costs of materials and labour required for projects",
  "CORE_277",  "Selling their products to purchasers",
  "CORE_335",  "Preparing simple or pre-prepared foods and beverages",
  "CORE_376",  "Patient Examination and Diagnosis",
  "CORE_390",  "Crop Harvesting and Post-Harvest Handling",
  "CORE_393",  "Monitoring Market Activity and Conditions, Determining Product Types and Quantities, and Planning and Coordinating Production",
  "CORE_398",  "Performing miscellaneous construction and building maintenance tasks",
  "OTHER_207", "Operating and monitoring machinery used to restrain, stun, slaughter animals and to trim carcasses into standard meat and fish cuts",
  "OTHER_218", "Researching the theoretical aspects and operational methods for the use of computers",
  "OTHER_255", "Appraising, Selecting, and Revising Material Submitted by Writers, Photographers, Illustrators, and Others to Create Favourable Publicity",
  "OTHER_377", "Distributing and Sending Letters, Information Sheets, and Other Documents to Clients",
  "OTHER_432", "Analysing Imagery, Documents, and Data to Design and Revise Maps and Charts",
  "OTHER_465", "Preparing and serving alcoholic and non-alcoholic drinks at a bar",
  "OTHER_77",  "Cleaning, Shaping and Polishing Nails, and Treating Minor Foot Ailments",
  "OTHER_80",  "Reading and interpreting specifications or following verbal instructions"
)

# ── 1) Utilities (safe helpers) ----------------------------------------------
.extract_l1_num <- function(x) {
  out <- suppressWarnings(as.integer(stringr::str_extract(x, "\\d+")))
  out
}

.safe_min_idx <- function(x) {
  # Return index of min non-NA; if all NA, return 1
  if (all(is.na(x))) return(1L)
  which.min(replace(x, is.na(x), Inf))
}

.choose_best_label <- function(df_in, source_tag) {
  cand <- df_in %>%
    dplyr::filter(label_source == source_tag,
                  !is.na(cluster_label), cluster_label != "")
  if (nrow(cand) == 0) return(NA_character_)
  cand %>%
    mutate(
      label_fit_score = suppressWarnings(as.numeric(label_fit_score)),
      label_fit_score = ifelse(is.na(label_fit_score), -Inf, label_fit_score)
    ) %>%
    add_count(cluster_label, name = "label_n") %>%
    arrange(desc(label_fit_score), desc(label_n), cluster_label) %>%
    slice(1) %>%
    pull(cluster_label) %>%
    as.character()
}

.longest_common_leading_words <- function(tasks, max_words = 20) {
  tasks <- tasks[!is.na(tasks) & tasks != ""]
  if (length(tasks) == 0) return(NA_character_)
  
  first_task_orig <- tasks[1]
  
  norm <- function(s) {
    s %>% str_replace_all("[^[:alnum:][:space:]]+", " ") %>%
      str_squish() %>% str_to_lower()
  }
  
  split_words <- str_split(norm(tasks), "\\s+")
  min_len <- min(lengths(split_words))
  max_check <- min(max_words, min_len)
  
  k <- 0L
  for (i in seq_len(max_check)) {
    ith <- map_chr(split_words, ~ .x[i])
    if (length(unique(ith)) == 1) k <- i else break
  }
  if (k == 0L) return(NA_character_)
  
  first_task_words <- str_split(first_task_orig, "\\s+")[[1]]
  phrase <- paste(first_task_words[seq_len(k)], collapse = " ")
  phrase %>%
    str_squish() %>% str_replace_all("\\s+", " ") %>% str_trim() %>%
    str_to_sentence()
}

# ── 2) Nest per-cluster to avoid vctrs issues --------------------------------
cluster_rows <- taxonomy_l2_l1labelled %>%
  group_by(unified_cluster_id) %>%
  summarise(rows = list(cur_data_all()), .groups = "drop")

# Derive cluster-level features from nested rows
cluster_base <- cluster_rows %>%
  mutate(
    l1_identifier_first = map_chr(rows, ~{
      l1s <- .x$l1_identifier
      idx <- .safe_min_idx(.extract_l1_num(l1s))
      l1s[idx]
    }),
    l1_num_min = map_int(rows, ~{
      vals <- .extract_l1_num(.x$l1_identifier)
      if (all(is.na(vals))) 999999L else suppressWarnings(min(vals, na.rm = TRUE))
    }),
    task_alpha_min = map_chr(rows, ~{
      ts <- .x$task
      ts <- ts[!is.na(ts)]
      if (length(ts) == 0) "" else min(ts)
    }),
    manual_label_from_rows = map_chr(rows, ~ .choose_best_label(.x, "Manual")),
    llm_label_from_rows    = map_chr(rows, ~ .choose_best_label(.x, "LLM")),
    auto_name_candidate    = map_chr(rows, ~ .longest_common_leading_words(.x$task, max_words = 20)),
    n_shared_words         = if_else(is.na(auto_name_candidate), 0L,
                                     as.integer(str_count(auto_name_candidate, "\\S+"))),
    cluster_methodology    = map_chr(rows, ~ dplyr::first(.x$methodology))
  ) %>%
  select(-rows)

# ── 3) Cross-methodology Level-2 numbering -----------------------------------
l2_index <- cluster_base %>%
  arrange(l1_num_min, task_alpha_min, unified_cluster_id) %>%
  mutate(l2_identifier = sprintf("l2_%04d", dplyr::row_number())) %>%
  select(unified_cluster_id, l2_identifier)

# ── 4) First-pass naming (Manual > LLM > Auto) --------------------------------
resolved_pass1 <- cluster_base %>%
  left_join(cluster_overrides, by = "unified_cluster_id") %>%
  mutate(
    proposed_label = coalesce(
      manual_override_label,
      manual_label_from_rows,
      llm_label_from_rows,
      auto_name_candidate
    ),
    source_after_pass1 = case_when(
      !is.na(manual_override_label)                                 ~ "Manual",
      is.na(manual_override_label) & !is.na(manual_label_from_rows) ~ "Manual",
      is.na(manual_override_label) & is.na(manual_label_from_rows) &
        !is.na(llm_label_from_rows)                                 ~ "LLM",
      TRUE                                                          ~ "Auto"
    )
  ) %>%
  select(unified_cluster_id, proposed_label, source_after_pass1, n_shared_words)

# ── 5) Auto review table: list all tasks, weakest prefixes first --------------
l2_auto_review_table <- taxonomy_l2_l1labelled %>%
  select(unified_cluster_id, l1_identifier, task) %>%
  left_join(l2_index, by = "unified_cluster_id") %>%
  left_join(resolved_pass1, by = "unified_cluster_id") %>%
  filter(source_after_pass1 == "Auto") %>%
  group_by (unified_cluster_id) %>%
  filter (n_distinct(l1_identifier) != 1) %>%
  ungroup () %>%
  arrange(n_shared_words,unified_cluster_id, task) %>%
  select(
    l2_identifier,
    unified_cluster_id,
    proposed_auto_label = proposed_label,
    n_shared_words,
    l1_identifier,
    task
  )

# ── 6) EDIT AFTER REVIEW: targeted replacements for Auto proposals ------------
# Fill this table ONLY for clusters present in l2_auto_review_table.
# Each row replaces the Auto first-N-words label with your chosen manual label.
auto_review_overrides <- tribble(
  ~unified_cluster_id, ~manual_override_label,
  "CORE_200", "Preparing and operating nets, lines and other fishing tackle and deck equipment",
  "CORE_209", "Controlling and maintaining radio communications, transmitting and broadcast systems",
  "CORE_222", "Preparing scholarly and scientific reports, papers and books",
  "CORE_325", "Storing for later use and carrying out some processing of produce",
  "CORE_413", "Inspecting and testing electrical and electronic systems, equipment, cables and machinery to identify hazards, defects, and the need for adjustment or repair",
  "CORE_9", "Applying Knowledge of Air Traffic Control, Flying, and Air Traffic Safety Engineering Principles and Practices to Identify and Solve Problems Arising in the Course of Work",
  "OTHER_150", "Applying Knowledge of Broadcasting, Telecommunications Terminals, Transmission Systems, and Telecommunications Engineering Principles and Practices to Identify and Solve Problems Arising in the Course of Work",
  "OTHER_322", "Promoting conferences, conventions and trade shows to potential customers", 
  "OTHER_394", "Monitoring market activity and planning production to meet contract requirements and market demand",
  "OTHER_416", "Advising individuals and organizations on government laws, rules and regulations concerning government benefit programmes and the determination and disbursement of payments or referral to services, as well as on the public’s rights and obligations",
  "OTHER_461", "Monitoring and Alleviating Pain and Discomfort Experienced by Patients, Including Women During Labour and Delivery, Using a Variety of Therapies Such as the Administration of Painkilling Drugs",
  "CORE_380", "Watering, thinning, weeding and tending crops by hand or using hand tools",
  "OTHER_237", "Conducting, Organizing and Supervising Aquaculture and Fishery Stock Examinations in Order to Identify Diseases or Parasites",
  "OTHER_487", "Filling bottles, cans, boxes, bags and other containers with products by hand",
  "OTHER_526", "Monitoring technical, regulatory and safety aspects of the construction, installation, operation, maintenance and repair of mineral ore, oil and natural gas exploration, extraction, transport and storage installations and of mineral processing plants",
  "OTHER_516", "Operating washing, separating, leaching, precipitating, filtering, extracting and combining equipment to remove waste material and recover minerals",
  "CORE_59", "Installing, monitoring and supporting the reliability and usability of internet and intranet websites or web server hardware or software",
  "OTHER_245", "Operating and monitoring screening equipment, bleaching equipment, digesters, mixing tanks, washers, and other pulp processing machinery and equipment to carry out one or more cellulose processing steps",
  "CORE_240", "Representing the organization in negotiations other agencies, and at conventions, seminars, public hearings and forums",
  "CORE_356", "Training and supervising workers in animal care procedures, maintenance duties and health and safety precautions, and hiring and discharging workers and contractors",
  "CORE_333", "Assisting in the preparation of budgets, monitoring of expenditures, drafting of contracts and purchasing or acquisition orders",
  "OTHER_340", "Setting up equipment for employee use, performing or ensuring proper installation of cables, operating systems or appropriate software")

# ── 7) Final naming with overrides applied ------------------------------------
resolved_final <- resolved_pass1 %>%
  left_join(auto_review_overrides, by = "unified_cluster_id") %>%
  mutate(
    l2_label = coalesce(manual_override_label, proposed_label),
    label_methodology = case_when(
      !is.na(manual_override_label)                      ~ "Manual",
      source_after_pass1 %in% c("Manual", "LLM")         ~ source_after_pass1,
      TRUE                                               ~ "Auto"
    )
  ) %>%
  select(unified_cluster_id, l2_label, label_methodology)

# ── 8) Assemble the final output ---------------------------------------------
taxonomy_l2_final <- resolved_final %>%
  left_join (l2_index, by = "unified_cluster_id") %>%
  left_join (taxonomy_l2_l1labelled %>% select (l1_identifier, "l1_label"=task, unified_cluster_id, "cluster_methodology" = methodology) %>% distinct (l1_label, .keep_all = TRUE), 
             by = "unified_cluster_id", relationship = "one-to-many") %>%
  select (l2_identifier, l2_label, cluster_methodology, label_methodology, l1_identifier, l1_label) %>%
  arrange (l2_identifier, l1_identifier)

l2_distinct <- n_distinct(taxonomy_l2_final$l2_identifier)

rm(list = setdiff(ls(), c("taxonomy_l2_final", "taxonomy_l1_final", "l1_distinct", "l2_distinct")))
