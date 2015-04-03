% vim: set expandtab softtabstop=4 shiftwidth=4:

-type tf_task_id() :: any().
-type tf_fun_ref() :: fun().
-type tf_fun_args() :: [any()].
-type tf_timeout() :: pos_integer().
-type tf_proplist(T1, T2) :: [{T1, T2}].
-type tf_task_result() :: any().

-record(tf_task, {
        id :: tf_task_id(),
        fun_ref :: tf_fun_ref(),
        args :: tf_fun_args(),
        timeout :: tf_timeout()
}).
-type tf_task() :: #tf_task{}.

-record(tf_bidding, {
        tasks :: [tf_task()],
        timeout :: tf_timeout()
}).
-type tf_bidding() :: #tf_bidding{}.

-record(tf_bidding_result, {
        completed :: tf_proplist(tf_task_id(), tf_task_result()),
        individual_timeouts :: [tf_task_id()],
        global_timeouts :: [tf_task_id()]
}).
-type tf_bidding_result() :: #tf_bidding_result{}.
