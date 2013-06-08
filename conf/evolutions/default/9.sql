# --- !Ups
ALTER TABLE `points` CHANGE `createdAt` `createdAt` DATETIME(6) NOT NULL;
ALTER TABLE `points` CHANGE `updatedAt` `updatedAt` DATETIME(6) NOT NULL;
ALTER TABLE `points` CHANGE `referencedAt` `referencedAt` DATETIME(6) NOT NULL;

# --- !Downs
ALTER TABLE `points` CHANGE `createdAt` `createdAt` DATETIME NOT NULL;
ALTER TABLE `points` CHANGE `updatedAt` `updatedAt` DATETIME NOT NULL;
ALTER TABLE `points` CHANGE `referencedAt` `referencedAt` DATETIME NOT NULL;