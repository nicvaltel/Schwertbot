
create table messages (
  id bigserial primary key not null,
  user_id bigint not null,
  text text not null,
  sent timestamp with time zone default (now() at time zone 'utc'),
  FOREIGN KEY (user_id) REFERENCES users(user_id)
);