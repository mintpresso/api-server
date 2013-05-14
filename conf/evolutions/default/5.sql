# --- !Ups

CREATE TABLE pointTypes (
  id bigint(20) NOT NULL PRIMARY KEY AUTO_INCREMENT,
  name varchar(200) NOT NULL
);

INSERT INTO pointTypes (name) VALUES ('post');
INSERT INTO pointTypes (name) VALUES ('user');
INSERT INTO pointTypes (name) VALUES ('page');

# --- !Downs

DROP TABLE pointTypes;