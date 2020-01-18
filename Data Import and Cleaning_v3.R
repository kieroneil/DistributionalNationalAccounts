library(tidyverse)
library(fs)
library(haven)
library(openxlsx)

# Get the paths for your subset of files. From the `fs` package
# Each file has ~68760 rows and 144 variables
paths <- dir_ls("Dina_subset/") 
paths

# Import all of the Stata dta files into a single object
# Also, put the year from the filename into a new column
dina_df <- map_dfr(paths, ~ read_dta(.x), .id = "filename") %>%
  extract(filename, "year", "(\\d{4})")


# Get the names of all the grouping variables
group_names <- names(dina_df[5:15])

# Change factor levels to decriptive labels 
# Assign to new vars and then drop originals and put at front
dina_df2 <- dina_df %>%
  mutate_at(group_names, as.character) %>%
  mutate(gender = if_else(female == "1", "Female", "Male", "Unknown"),
         agegroup_primary = recode(ageprim, "0" = "20-64", "20" = "20-44", 
                                   "45" = "45-64", "65" = "65 Plus"),
         agegroup_secondary = recode(agesec, "0" = "20-64", "20" = "20-44", 
                                     "45" = "45-64", "65" = "65 Plus"), 
         agegroup_imputed = recode(age, "0" = "20-64", "20" = "20-44", 
                                   "45" = "45-64", "65" = "65 Plus"), 
         labor_status_primary = if_else(oldexm == "1", "Retired", "Working", "Unknown"),
         labor_status_secondary = if_else(oldexf == "1", "Retired", "Working", "Unknown"),
         labor_status_imputed = if_else(oldexf == "1", "Retired", "Working", "Unknown"), 
         filing_status = if_else(married == "1", "Married", "Single", "Unknown"), 
         earner_status = if_else(second == "0", "Primary", "Secondary", "Unknown"), 
         num_kids = xkidspop, 
         filer_status = if_else(filer == "1", "Filer", "Not filer", "Unknown")
  ) %>%
  select(-group_names) %>%
  select(year, everything())

# Get the names of new grouping variables
grouping_vars <- names(dina_df2[136:146])

# Give columns more descriptive names
# I try to use consistent names so that you will be able to efficiently select 
#  column names with `contains()`.
dina_df3 <- dina_df2 %>%
  #select(id, group_names, everything()) %>%
  mutate_at(grouping_vars, as.factor) %>%
  rename(
    tax_unit_id = id,
    population_weight = dweght,
    population_weight_ptu = dweghttaxu,
    
    ttl_income_national_factor = princ,
    ttl_income_national_pretax = peinc,
    ttl_income_national_posttax = poinc,
    ttl_income_personal_pretax = ptinc,
    ttl_income_personal_pretax_labor = plinc,
    ttl_income_personal_pretax_capital = pkinc,
    ttl_income_personal_disposable_extended = diinc,
    ttl_income_personal_factor = fainc,
    ttl_income_personal_factor_labor = flinc,
    ttl_income_personal_factor_capital = fkinc,
    ttl_income_fiscal_excl_capgains = fiinc,
    ttl_income_fiscal_incl_capgains = fninc,
    ttl_income_social_insurance = ben, #Total benefits (cash + kind + coll, excl. pensions, UI, DI)
    ttl_wealth_personal_net = hweal,
    ttl_ecc_expenditure_consumption_collective = colexp,
    ttl_taxes_payments_contributions = tax,
    
    income_personal_factor_labor_wages = flemp,
    income_personal_factor_labor_mixed = flmil,
    income_personal_factor_labor_sales_taxes = flprl,
    income_personal_factor_capital_housing = fkhou,
    income_personal_factor_capital_equity = fkequ,
    income_personal_factor_capital_interest = fkfix,
    income_personal_factor_capital_business = fkbus,
    income_personal_factor_capital_pension_insurance = fkpen,
    income_fiscal_wages_pensions = fiwag,
    income_fiscal_business = fibus,
    income_fiscal_rents = firen,
    income_fiscal_interest = fiint,
    income_fiscal_dividends = fidiv,
    income_fiscal_capital_gains = fikgi,
    income_fiscal_nonfiler_default = fnps,
    income_pension_taxable = peninc,
    income_schedule_net = schcinc,
    income_s_corp_net = scorinc,
    income_partnership_net = partinc,
    income_rental_net = rentinc,
    income_estate_trust_net = estinc,
    income_royalty_net = rylinc,
    income_other_in_agi = othinc,
    income_capital_main_house_asset = fkhoumain,
    income_capital_rental_house = fkhourent,
    income_investment_payable_pensions = pkpen,
    income_social_insurance_labor_share = plbel,
    income_social_insurance_capital_share = pkbek,
    income_personal_labor_pension_pretax = plnin,
    income_personal_capital_pension_pretax = pknin,
    income_personal_pension_pretax = ptnin, 
    income_cash_disposable = dicsh,
    income_transfers_in_kind_social = inkindinc, 
    income_investment_pensions_payable = invpen,
    income_social_insurance = plben,
    income_social_insurance_oldage = ssinc_oa,
    income_social_insurance_disability = ssinc_di,
    income_social_insurance_unemployment = uiinc,
    income_social_insurance_cash = dicab, 
    income_social_insurance_taxcredit = dicred,
    income_social_insurance_foodstamps = difoo,
    income_social_insurance_veterans = disup,
    income_social_insurance_workerscomp = divet,
    income_social_insurance_othercash = dicao,
    income_social_insurance_tanf = tanfinc,
    income_social_insurance_cashlocalstate = othben,
    income_social_insurance_medicare = medicare,
    income_social_insurance_medicaid = medicaid,
    income_social_insurance_inkind = otherkin,
    income_social_insurance_pell = pell,
    income_social_insurance_veteraninkind = vethealth,
    
    assets_equity = hwequ,
    assets_currency = hwfix,
    assets_housing = hwhou,
    assets_business = hwbus,
    assets_pension_life_insurance = hwpen,
    
    liabilities_household = hwdeb,
    
    wages_all_filers_taxable = flwag,
    wages_all_filers_taxable_supplements = flsup,
    
    benefits_pension = plpbe,
    benefits_di_ui = plobe,
    benefits_pension_capital_share = pkpbk, 
    benefits_pension_labor_share = plpbl,
    
    ecc_net_property_income_paid_by_govt = govin, 
    ecc_net_income_non_profit = npinc, 
    ecc_education = educ, 
    ecc_education_2 = colexp2, 
    
    surplus_primary_public_pension_system = prisupen,
    surplus_primary_private_pension_system = prisupenprivate,
    surplus_primary_government = prisupgov,
    #poinc2 = poinc2,
    
    taxes_capital_sales_excise = fkprk,
    taxes_property_housing = proprestax,
    taxes_property_business = propbustax,
    taxes_personal_income_wealth_current = ditax,
    taxes_personal_federal_income = ditaf,
    taxes_personal_state_income = ditas,
    taxes_sales_excise = salestax, 
    taxes_corporate = corptax,
    taxes_estate = estatetax,
    
    payments_interest = fkdeb,
    payments_interest_mortgage = fkmor,
    payments_interest_nonmortgage = fknmo,
    
    contributions_pension_minus = plpco,
    contributions_social_insurance_di_ui = ploco,
    contributions_social_insurance_govt = govcontrib, 
    contributions_social_insurance = ssuicontrib,
    contributions_social_insurance_other = othercontrib,
    contributions_health_insurance = waghealth,
    contributions_pension = wagpen,
    contributions_social = plcon,
    
    wealth_personal_not_cap_net = hwealnokg,
    wealth_rental_housing_gross = rentalhome,
    wealth_rental_housing_mortgages = rentalmort,
    wealth_rental_housing_net = rental,
    wealth_main_housing_gross = ownerhome,
    wealth_main_housing_mortgages = ownermort, 
    wealth_main_housing_net = housing,
    wealth_partnership = partw, 
    wealth_sole_proprietor = soleprop, 
    wealth_s_corp = scorw,
    wealth_equity = equity, 
    wealth_taxable_bond = taxbond, 
    wealth_muni_bond = muni,
    wealth_currency = currency, 
    wealth_non_mortgage_debt = nonmort,
    wealth_household_financial_assets = hwfin,
    wealth_household_nonfinancial_assets = hwnfa
  ) 


# Reconciliation and stuff
# * For some reason the population is multiplied by 100,000. I divide by 100,000 so that the 
#     sum of the population weights in any given year will equal the actual population
#     size in the year.
dina_df4 <- dina_df3 %>%
  select(tax_unit_id, grouping_vars, everything()) %>%
  mutate(
    year = factor(year),
    population_weight = population_weight / 100000,
    population_weight_ptu = population_weight_ptu / 100000)

saveRDS(dina_df4, "Transform_checkpoint.RDS")

dina_df5 <- dina_df4 %>%
  mutate(
    summ_income_personal_factor_labor = 
      income_personal_factor_labor_wages + 
      income_personal_factor_labor_mixed +
      income_personal_factor_labor_sales_taxes, #flemp + flmil + flprl
    recon_income_personal_factor_labor = 
      ttl_income_personal_factor_labor - summ_income_personal_factor_labor,
    
    summ_income_personal_factor_capital = 
      income_personal_factor_capital_housing +
      income_personal_factor_capital_equity +
      income_personal_factor_capital_interest +
      income_personal_factor_capital_business +
      income_personal_factor_capital_pension_insurance +
      payments_interest, #fkhou + fkequ + fkfix + fkbus + fkpen + fkdeb
    recon_income_personal_factor_capital = 
      ttl_income_personal_factor_capital - summ_income_personal_factor_capital,
    
    summ_ttl_income_personal_factor = 
      summ_income_personal_factor_labor +
      summ_income_personal_factor_capital, #flinc + fkinc
    recon_income_personal_factor = 
      ttl_income_personal_factor - summ_ttl_income_personal_factor,
    
    summ_income_personal_pretax_labor = 
      summ_income_personal_factor_labor +
      contributions_social +
      income_social_insurance_labor_share, #flinc + plcon + plbel
    recon_income_personal_pretax_labor = 
      ttl_income_personal_pretax_labor - summ_income_personal_pretax_labor,

    summ_income_personal_pretax_capital = 
      summ_income_personal_factor_capital +
      income_investment_payable_pensions +
      income_social_insurance_capital_share, #fkinc + pkpen + pkbek
    recon_income_personal_pretax_capital = 
      ttl_income_personal_pretax_capital - summ_income_personal_pretax_capital,
    
    summ_ttl_income_personal_pretax = 
      summ_income_personal_pretax_labor +
      summ_income_personal_pretax_capital, #plinc + pkinc
    recon_ttl_income_personal_pretax = 
      ttl_income_personal_pretax - summ_ttl_income_personal_pretax,
      
    summ_income_fiscal_incl_capgains = 
      income_fiscal_wages_pensions +
      income_fiscal_business + 
      income_fiscal_rents + 
      income_fiscal_interest +
      income_fiscal_dividends, #fiwag + fibus + firen + fiint + fidiv
    recon_income_fiscal_incl_capgains = 
      ttl_income_fiscal_incl_capgains - summ_income_fiscal_incl_capgains,
    
    summ_income_fiscal_excl_capgains = 
      income_fiscal_wages_pensions +
      income_fiscal_business + 
      income_fiscal_rents + 
      income_fiscal_interest +
      income_fiscal_dividends + 
      income_fiscal_capital_gains, #fiwag + fibus + firen + fiint + fidiv + fikgi
    recon_income_fiscal_excl_capgains = 
      ttl_income_fiscal_excl_capgains - summ_income_fiscal_excl_capgains,
     
    summ_income_personal_disposable_extended = 
      income_cash_disposable +
      income_transfers_in_kind_social +
      ttl_ecc_expenditure_consumption_collective, #dicsh + inkindinc + colexp
    recon_income_personal_disposable_extended = 
      ttl_income_personal_disposable_extended - summ_income_personal_disposable_extended,
    
    summ_income_national_factor = 
      summ_ttl_income_personal_factor + 
      ecc_net_property_income_paid_by_govt +
      ecc_net_income_non_profit, #fainc + govin + npinc
    recon_income_national_factor = 
      ttl_income_national_factor - summ_income_national_factor,
    
    summ_income_national_pretax = 
      summ_ttl_income_personal_pretax +
      ecc_net_property_income_paid_by_govt +
      ecc_net_income_non_profit +
      surplus_primary_public_pension_system +
      income_investment_pensions_payable, #ptinc + govin + npinc + prisupen + invpen
    recon_income_national_pretax = 
      ttl_income_national_pretax - summ_income_national_pretax,
    
    summ_income_national_posttax = 
      summ_income_personal_disposable_extended +
      ecc_net_property_income_paid_by_govt +
      ecc_net_income_non_profit +
      surplus_primary_private_pension_system +
      income_investment_pensions_payable +
      surplus_primary_government, # diinc + govin + npinc + prisupenprivate + invpen + prisupgov
    recon_income_national_posttax =
      ttl_income_national_posttax - summ_income_national_posttax,
    
    summ_wealth_personal_net = 
      assets_equity +
      assets_currency +
      assets_housing +
      assets_business +
      assets_pension_life_insurance +
      liabilities_household, #hwequ + hwfix + hwhou + hwbus + hwpen + hwdeb
    recon_wealth_personal_net = 
      ttl_wealth_personal_net - summ_wealth_personal_net,
    
    summ_cohort_income_tax_rate = if_else(
      summ_income_national_pretax != 0, 
      1 - (summ_income_national_posttax / summ_income_national_pretax), 
      0),

    summ_income_national_perperson_pretax = summ_income_national_pretax / population_weight,
    summ_income_national_perperson_posttax = summ_income_national_posttax / population_weight,
    summ_income_perperson_tax_rate = if_else(
      summ_income_national_perperson_pretax != 0, 
      1 - (summ_income_national_perperson_posttax / summ_income_national_perperson_pretax), 
      0),
    recon_tax_rates = 
      summ_cohort_income_tax_rate - summ_income_perperson_tax_rate
)

dina_final <- dina_df5

# Export to other formats
saveRDS(dina_final, "dina_data.RDS")
write.xlsx(dina_final, "dina_data.xlsx", asTable = TRUE) #Not recommended for larger datasets

#========================
# Recon section
#
dina_final <- readRDS("dina_data.RDS")

dina_recon <- dina_final %>%
  filter(year == 2018) %>%
  select(starts_with("recon")) 

summary(dina_recon)

# Everything reconciles!!!!
  
