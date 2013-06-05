# --- !Ups
CREATE TABLE `edges` (
    `id` bigint(20) NOT NULL PRIMARY KEY AUTO_INCREMENT,
    `sId` bigint(20) NOT NULL,
    `sType` bigint(20) NOT NULL,
    `v` varchar(200) NOT NULL,
    `oId` bigint(20) NOT NULL,
    `oType` bigint(20) NOT NULL,
    `createdAt` datetime NOT NULL
);

# --- !Downs
DROP TABLE IF EXISTS `edges`;