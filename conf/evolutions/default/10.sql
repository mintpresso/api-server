# --- !Ups
ALTER TABLE `edges` CHANGE `createdAt` `createdAt` DATETIME(6) NOT NULL;

# --- !Downs
ALTER TABLE `edges` CHANGE `createdAt` `createdAt` DATETIME NOT NULL;