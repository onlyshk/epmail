%%%-------------------------------------------------------------------
%%% @author  Kuleshov Alexander
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 18 Feb 2011 by  <kuleshovmail@gmail.com>
%%%-------------------------------------------------------------------

{application, epmail,
  [{description, "Erlang mail system"},
  {vsn, "0.2"},
  {modules, [epmail, epmail_sup, smtp_fsm_sup, utils, smtp_messages, popd_listener, smtpd_listener,
             maildir, mime, config, smtpd_listener_sup, pop_messages, logger, pop_fsm_sup, logger_sup, smtp_fsm,
             popd_fsm, popd_listener_sup]},
  {registered, [epmail_sup, popd_listener_sup, smtpd_listener_sup, pop_fsm_sup, smtp_fsm_sup, 
               popd_listener, smtp_listener]},
  {applications, [kernel, stdlib]},
  {mod, {epmail, []}},
  {env, [{config, "epmail.conf.sample"}]}
]}.
