# --- !Ups

CREATE TABLE point_types (
  id bigint(20) NOT NULL PRIMARY KEY AUTO_INCREMENT,
  name varchar(255) NOT NULL
);

# --- !Downs

DROP TABLE point_types;
