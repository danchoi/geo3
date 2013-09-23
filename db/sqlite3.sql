create table events (
  event_user text,
  event_log text,
  event_created_at datetime
);

create table users (
  user_name text UNIQUE,
  user_lat real,
  user_lng real,
  user_zoom num
);

create table posts (
  user_name text,
  post_lat real,
  post_lng real,
  post_zoom num,
  post_created_t datetime
);
