create table posts (
  session integer,
  post_author text,
  post_lat real,
  post_lng real,
  post_zoom integer,
  post_text text,
  post_created_at datetime default current_timestamp
);

create table sessions (
  session integer primary key,
  session_nickname text,
  session_lat real,
  session_lng real,
  session_zoom integer,
  session_created_at datetime default current_timestamp,
  session_updated_at datetime default current_timestamp,
  session_disconnected_at datetime, 
  session_opaque_uuid text UNIQUE
);

