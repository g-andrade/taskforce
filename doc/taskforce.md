

# Module taskforce #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-execution_result">execution_result()</a> ###


<pre><code>
execution_result() = {Completed::<a href="#type-tf_proplist">tf_proplist</a>(<a href="#type-tf_task_id">tf_task_id()</a>, <a href="#type-tf_task_result">tf_task_result()</a>), IndividualTimeouts::[<a href="#type-tf_task_id">tf_task_id()</a>], GlobalTimeouts::[<a href="#type-tf_task_id">tf_task_id()</a>]}
</code></pre>




### <a name="type-tf_fun_args">tf_fun_args()</a> ###


<pre><code>
tf_fun_args() = [any()]
</code></pre>




### <a name="type-tf_fun_ref">tf_fun_ref()</a> ###


<pre><code>
tf_fun_ref() = function()
</code></pre>




### <a name="type-tf_proplist">tf_proplist()</a> ###


<pre><code>
tf_proplist(T1, T2) = [{T1, T2}]
</code></pre>




### <a name="type-tf_task">tf_task()</a> ###


<pre><code>
tf_task() = #tf_task{id = <a href="#type-tf_task_id">tf_task_id()</a>, fun_ref = <a href="#type-tf_fun_ref">tf_fun_ref()</a>, args = <a href="#type-tf_fun_args">tf_fun_args()</a>, timeout = <a href="#type-tf_timeout">tf_timeout()</a>}
</code></pre>




### <a name="type-tf_task_id">tf_task_id()</a> ###


<pre><code>
tf_task_id() = any()
</code></pre>




### <a name="type-tf_task_result">tf_task_result()</a> ###


<pre><code>
tf_task_result() = any()
</code></pre>




### <a name="type-tf_timeout">tf_timeout()</a> ###


<pre><code>
tf_timeout() = pos_integer()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#execute_tasks-1">execute_tasks/1</a></td><td></td></tr><tr><td valign="top"><a href="#execute_tasks-2">execute_tasks/2</a></td><td></td></tr><tr><td valign="top"><a href="#execute_tasks-3">execute_tasks/3</a></td><td></td></tr><tr><td valign="top"><a href="#new_task-4">new_task/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="execute_tasks-1"></a>

### execute_tasks/1 ###

<pre><code>
execute_tasks(Tasks::[<a href="#type-tf_task">tf_task()</a>]) -&gt; <a href="#type-execution_result">execution_result()</a>
</code></pre>
<br />

<a name="execute_tasks-2"></a>

### execute_tasks/2 ###

<pre><code>
execute_tasks(Tasks::[<a href="#type-tf_task">tf_task()</a>], TotalTimeout::<a href="#type-tf_timeout">tf_timeout()</a>) -&gt; <a href="#type-execution_result">execution_result()</a>
</code></pre>
<br />

<a name="execute_tasks-3"></a>

### execute_tasks/3 ###

<pre><code>
execute_tasks(Tasks::[<a href="#type-tf_task">tf_task()</a>], TotalTimeout::<a href="#type-tf_timeout">tf_timeout()</a>, MaxMinionCount::pos_integer()) -&gt; <a href="#type-execution_result">execution_result()</a>
</code></pre>
<br />

<a name="new_task-4"></a>

### new_task/4 ###

<pre><code>
new_task(Id::<a href="#type-tf_task_id">tf_task_id()</a>, FunRef::<a href="#type-tf_fun_ref">tf_fun_ref()</a>, Args::<a href="#type-tf_fun_args">tf_fun_args()</a>, Timeout::<a href="#type-tf_timeout">tf_timeout()</a>) -&gt; <a href="#type-tf_task">tf_task()</a>
</code></pre>
<br />

