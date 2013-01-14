# --- !Ups
CREATE TABLE edges (
    id bigint(20) NOT NULL PRIMARY KEY AUTO_INCREMENT,
    sId bigint(20) NOT NULL,
    sType tinyint(20) NOT NULL,
    v varchar(255) NOT NULL,
    oId bigint(20) NOT NULL,
    oType tinyint(20) NOT NULL,
    createdAt date NOT NULL
);

# --- !Downs
DROP TABLE edges IF EXISTS;