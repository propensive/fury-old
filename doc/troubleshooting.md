# Troubleshooting

Fury is still experimental, and while current versions should work consistently for most users, without issues,
it is still known to encounter problems occasionally whereby it becomes unresponsive, and commands don't work or
the server refuses to stop.

Until these bugs are fixed, it's useful to know how to resolve these situations.

## Restart Fury

Most problems can be resolved by restarting Fury.

You should be able to see if Fury is running with,
```sh
pgrep fury
```

Running,
```sh
fury stop
```
should terminate this process, but if it still appears after running `fury stop`, then it might be necessary to
terminate it by running,
```sh
killall fury
```
or, for systems which don't have `killall`,
```sh
pgrep '^fury$' | xargs kill
```

If that fails, a more forceful termination can be brought about with,
```sh
pgrep '^fury$' | xargs kill -9
```

Note that if Fury was installed on a system without a C compiler (`cc`) on the path, its process will be called
`java` instead of `fury`, and more care will need to be taken in killing the correct processes.

### Delete temporary files

Temporary files in a layer directory have been known to sometimes cause problems when running Fury. If Fury is
consistently hanging during operations on a particular layer, first stop Fury (as described above) and then
delete its temporary files from the project directory with,
```sh
rm -r .bloop .fury/*/
```

This will delete Fury's temporary files, which can all be recreated (provided there is an Internet connection)
by running Fury.

Note, however, that the trailing `/` in this command is very important, as we do not want to delete the layer's
configuration file, `.fury/config`.

### External processes

Sometimes, external processes can be responsible for Fury failing, and in addition to terminating Fury it may
also be necessary to terminate these other long-running processes.

In particular, Bloop and IPFS are relied upon heavily by Fury, and killing them may hely to recover.

## Diagnosis

While it is useful to know how to recover from a problem with Fury, it's more useful to the development of Fury
to identify the cause of the failure.

Fury's logs are typically stored in `~/.cache/fury/logs/`, one file per day, named by date. The last few lines
of the log may reveal more information about the cause of a failure, for example,
```sh
tail -n 20 ~/.cache/fury/logs/2020-03-14
```

### Reporting issues

Fury's [issue tracker](https://github.com/propensive/fury/issues) welcomes reports of any unexpected behavior in
Fury. Please include as much information in the issue as you can, though short and precise reports are welcome
if they contain all the information necessary to repeat the issue.

In any case, a bad issue report is better than no report: Fury's developers may request more information if it
is not provided, and if there is not enough information to replicate the issue, it may be closed after some
time.

### Diagnostic stack traces

If Fury is still running, but unresponsive, it can be very useful to see stack traces from Fury's threads to try
to determine what activity is causing it to get stuck.

Firstly, we need to find out Fury's process ID. The `jps` utility (bundled with the JDK) can help. Running,
```sh
jps
```
will produce a short list of Java processes IDs, alongside their main class. For example,
```
5827 Fury
8746 Jps
```

In this example, we need process ID `5827`.

Stack traces from all tasks can be obtained with,
```sh
jstack 5827 > stacks.txt
```

The file, `stacks.txt`, may provide very useful information in any bug report, and should 
