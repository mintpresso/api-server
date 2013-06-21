# --- !Ups
ALTER TABLE `edges` ADD COLUMN `data` VARCHAR(10240) NOT NULL;
ALTER TABLE `edges` ADD COLUMN `updatedAt` DATETIME(6) NOT NULL;

SET SQL_SAFE_UPDATES = 0;
UPDATE `edges` set `data` = "{}" where `data` = "";
UPDATE `edges` set `updatedAt` = `createdAt`;
SET SQL_SAFE_UPDATES = 1;
 
# --- !Downs
ALTER TABLE `edges` DROP COLUMN `data`;
ALTER TABLE `edges` DROP COLUMN `updatedAt`;