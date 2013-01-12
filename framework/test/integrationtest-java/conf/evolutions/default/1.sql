# --- Created by Ebean DDL
# To stop Ebean DDL generation, remove this comment and start using Evolutions

# --- !Ups

create table hello_world (
  id                        bigint not null,
  field1                    varchar(255),
  field2                    varchar(255),
  constraint pk_hello_world primary key (id))
;

create table jcustomer (
  id                        bigint not null,
  email                     varchar(255),
  constraint pk_jcustomer primary key (id))
;

create table jorder (
  id                        bigint not null,
  customer_id               bigint,
  date                      timestamp,
  constraint pk_jorder primary key (id))
;

create table jorder_item (
  id                        bigint not null,
  order_id                  bigint,
  qty                       integer,
  product_code              varchar(255),
  delivery_date             timestamp,
  constraint pk_jorder_item primary key (id))
;

create sequence hello_world_seq;

create sequence jcustomer_seq;

create sequence jorder_seq;

create sequence jorder_item_seq;

alter table jorder add constraint fk_jorder_customer_1 foreign key (customer_id) references jcustomer (id) on delete restrict on update restrict;
create index ix_jorder_customer_1 on jorder (customer_id);
alter table jorder_item add constraint fk_jorder_item_order_2 foreign key (order_id) references jorder (id) on delete restrict on update restrict;
create index ix_jorder_item_order_2 on jorder_item (order_id);



# --- !Downs

SET REFERENTIAL_INTEGRITY FALSE;

drop table if exists hello_world;

drop table if exists jcustomer;

drop table if exists jorder;

drop table if exists jorder_item;

SET REFERENTIAL_INTEGRITY TRUE;

drop sequence if exists hello_world_seq;

drop sequence if exists jcustomer_seq;

drop sequence if exists jorder_seq;

drop sequence if exists jorder_item_seq;

