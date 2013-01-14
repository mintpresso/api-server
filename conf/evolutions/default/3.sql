# --- !Ups
CREATE TABLE points (
    id bigint(20) NOT NULL PRIMARY KEY AUTO_INCREMENT,
    accountId bigint(20) NOT NULL,
    identifier varchar(1000) NOT NULL UNIQUE,
    typeId tinyint(20) NOT NULL,
    createdAt date NOT NULL,
    updatedAt date NOT NULL,
    referencedAt date NOT NULL,
    data varchar(10240) NOT NULL,
);

# --- !Downs
DROP TABLE points IF EXISTS;