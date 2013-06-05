# --- !Ups
ALTER TABLE `pointTypes` ADD UNIQUE ( `name` );
ALTER TABLE `points` ADD INDEX ( `accountId` );
ALTER TABLE `edges` ADD INDEX ( `accountId` );

# --- !Downs
ALTER TABLE `pointTypes` DROP INDEX ( `name` );
ALTER TABLE `points` DROP INDEX ( `accountId` );
ALTER TABLE `edges` DROP INDEX ( `accountId` );