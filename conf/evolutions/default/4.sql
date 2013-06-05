# --- !Ups
ALTER TABLE `accounts` ADD COLUMN `api_token` varchar(1000);

# --- !Downs
ALTER TABLE `accounts` DROP COLUMN `api_token`;