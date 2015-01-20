begin;

create table if not exists cloudfront_log_files (
  file_id serial not null primary key,
  md5 text not null unique,
  file_name text,
  added_time timestamp not null default current_timestamp
);

create table if not exists cloudfront_log_records (
  file_id integer not null references cloudfront_log_files (file_id),
  date date,
  time time,
  x_edge_location text,
  sc_bytes bigint,
  c_ip inet,
  cs_method text,
  cs_host text,
  cs_uri_stem text,
  sc_status integer,
  cs_referer text,
  cs_user_agent text,
  cs_uri_query text,
  cs_cookie text,
  x_edge_result_type  text,
  x_edge_request_id   text,
  x_host_header       text,
  cs_protocol         text,
  cs_bytes            bigint,
  time_taken          interval
  );

create view cloudfront_log_date_records as
select *,
date_trunc('month', date)::date as month,
date_trunc('year', date)::date as year
from cloudfront_log_records;
  
commit;
