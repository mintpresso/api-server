# --- !Ups

CREATE TABLE pointTypes (
  id bigint(20) NOT NULL PRIMARY KEY AUTO_INCREMENT,
  name varchar(255) NOT NULL
);

# --- !Downs

DROP TABLE pointTypes;