

# Module taskforce #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-execution_options">execution_options()</a> ###


<pre><code>
execution_options() = #{timeout =&gt; pos_integer(), max_workers =&gt; pos_integer()}
</code></pre>




### <a name="type-result">result()</a> ###


<pre><code>
result() = #{completed =&gt; #{TaskId::term() =&gt; TaskResult::term()}, individual_timeouts =&gt; [TaskId::term()], global_timeouts =&gt; [TaskId::term()]}
</code></pre>




### <a name="type-result">result()</a> ###


<pre><code>
result() = #{completed =&gt; #{TaskId::term() =&gt; TaskResult::term()}, individual_timeouts =&gt; [TaskId::term()], global_timeouts =&gt; [TaskId::term()]}
</code></pre>




### <a name="type-task">task()</a> ###


__abstract datatype__: `task()`




### <a name="type-task_settings">task_settings()</a> ###


<pre><code>
task_settings() = #{timeout =&gt; pos_integer()}
</code></pre>




### <a name="type-task_settings">task_settings()</a> ###


<pre><code>
task_settings() = #{timeout =&gt; pos_integer()}
</code></pre>




### <a name="type-tasks">tasks()</a> ###


<pre><code>
tasks() = #{TaskId::term() =&gt; Task::<a href="#type-task">task()</a>}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#execute-1">execute/1</a></td><td></td></tr><tr><td valign="top"><a href="#execute-2">execute/2</a></td><td></td></tr><tr><td valign="top"><a href="#task-3">task/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="execute-1"></a>

### execute/1 ###

<pre><code>
execute(Tasks) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Tasks = <a href="#type-tasks">tasks()</a></code></li><li><code>Result = <a href="#type-result">result()</a></code></li></ul>

<a name="execute-2"></a>

### execute/2 ###

<pre><code>
execute(Tasks, ExecutionOptions) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Tasks = <a href="#type-tasks">tasks()</a></code></li><li><code>ExecutionOptions = <a href="#type-execution_options">execution_options()</a></code></li><li><code>Result = <a href="#type-result">result()</a></code></li></ul>

<a name="task-3"></a>

### task/3 ###

<pre><code>
task(Function, Args, TaskSettings) -&gt; Task
</code></pre>

<ul class="definitions"><li><code>Function = function()</code></li><li><code>Args = [term()]</code></li><li><code>TaskSettings = <a href="#type-task_settings">task_settings()</a></code></li><li><code>Task = <a href="#type-task">task()</a></code></li></ul>

