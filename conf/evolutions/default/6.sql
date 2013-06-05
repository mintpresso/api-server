# --- !Ups
ALTER TABLE `edges` ADD COLUMN `accountId` bigint(20) NOT NULL;

# --- !Downs
ALTER TABLE `edges` DROP COLUMN `accountId`;