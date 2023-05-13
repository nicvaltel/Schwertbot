create extension citext;
create extension pgcrypto;

create table users (
  user_id bigint primary key not null,
  username text not null,
  created timestamp with time zone default (now() at time zone 'utc')
);