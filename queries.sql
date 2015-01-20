
-- Top N from the last month

select
  count(*) as downloads,
  split_part(cs_uri_stem, '/', 3) as project,
  date_trunc('month', current_date - interval '1 month') as month
from
  cloudfront_log_records
where
  cs_uri_stem like '/archive/%' and
  date_trunc('month', date) = date_trunc('month', current_date - interval '1 month')
group by
  split_part(cs_uri_stem, '/', 3),
  date_trunc('month', current_date - interval '1 month')
order by
  1 desc
limit 50;


-- Month by month for a given project

select
  count(*) as downloads,
  split_part(cs_uri_stem, '/', 3) as project,
  date_trunc('month', date) as month
from 
