# --- !Ups
CREATE TABLE points (
    id bigint(20) NOT NULL PRIMARY KEY AUTO_INCREMENT,
    accountId bigint(20) NOT NULL,
    identifier varchar(1000) NOT NULL UNIQUE,
    typeId bigint(20) NOT NULL,
    createdAt datetime NOT NULL,
    updatedAt datetime NOT NULL,
    referencedAt datetime NOT NULL,
    data varchar(10240) NOT NULL,
);

# --- !Downs
DROP TABLE points IF EXISTS;