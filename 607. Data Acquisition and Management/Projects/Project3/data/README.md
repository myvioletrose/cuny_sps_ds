################
# job_position #
################ 
# fcc_unique_id is the primary key

1  fcc_unique_id
2      job_title
3 queried_salary
4       job_type
5          skill
6     company_id

#####################
# job_post_specific #
#####################
# jk_id is the primary key

1             jk_id
2     fcc_unique_id
3              link
4 date_since_posted
5          location

###########
# company #
###########
# company_id is the primary key

1        company_id
2           company
3   company_revenue
4 company_employees
5  company_industry
6       no_of_stars
7     no_of_reviews

###############
# description #
###############
# link is not technically a key but it is a unique pointer in the table

1        link
2 description