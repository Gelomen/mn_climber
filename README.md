mn_climber
=====
从rabbitmq server的代码中,分离了与`mnesia`相关的模块,同时去除了集群的相关代码

## 使用
只需将`mn_climber`加入到应用的依赖应用列表即可,并且将`{use_mn_climber, true}`加入到应用的`env`

## auto_init开关说明
```erlang
{auto_init, true} % 打开开关(默认)
{auto_init, false} % 关闭开关
```
打开开关之后,当本应用启动时会检查所有应用的模块,模块的`behavior(behaviour)`设置为`mn_climber_table`会被视为`表定义模块`,格式如下:
```erlang
-behavior(mn_climber_table).

table_definitions() ->
    [
        {mn_parent, [
            {disc_copies, [node()]},
            {record_name, mn_parent},
            {attributes, record_info(fields, mn_parent)},
            {match, #mn_parent{children = #children{_ = '_'}, _ = '_'}}
        ]}
    ].
```
`mn_climber_table:definitions()`函数会调用所有`表定义模块`的方法`table_definitions/0`来获取表定义;

之后相关函数会获取到`mneisa`数据库里面的所有表然后与代码里面的表定义`mn_climber_table:definitions()`对比,做如下的检查:
* 检查1:如果代码定义里面没有这张表,将会根据代码里的定义创建这个表.
* 检查2:如果数据库的表record数据结构跟代码里的定义不一样,抛出异常.
* 检查3:拿出每一张表里面的第一条数据,然后再拿出代码里面表定义的match项,这是一个自定义的配置,并不是mnesia支持的表配置,在此仅仅是用来检查数据内容是否跟代码定义的一样,其原理利用的是`ets:match_spec_run`检查配置跟内容是否匹配.

以上的检查目的主要是为了mneisa的表内容与代码定义的一致.

## auto_upgrade开关说明
```erlang
{auto_upgrade, true} % 打开开关(默认)
{auto_upgrade, false} % 关闭开关
```
打开开关之后,当本应用启动时会检查所有应用的模块,模块包含名为`mn_climber_upgrade`的attribute会被视为更新操作,格式如下
```erlang
% Name :: atom().
% Requires :: [atom()].
-mn_climber_upgrade({Name, Requires}).

'Name'() -> ok.
```
`Name`是操作的名字,同时也是执行操作的函数名,当操作需要执行的时候,会调用本模块下的与`Name`同名的函数名,因此,这个函数必须导出,参数个数为0,返回值必须为`ok`;
`Requires`是当前操作的前置操作名列表,如果指定了这个值,当前操作将会在`Requires`列表里的操作执行后才会执行当前更新操作.

## 当运行有mnesia的node需要重命名时
未完成
