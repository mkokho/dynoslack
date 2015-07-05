Scheduling Algorithms ![build status](https://travis-ci.org/mkokho/dynoslack.svg?branch=master)
=====================

This project implements several scheduling algorithms related to theoretical research in scheduling theory.
In particular, it contains implementations of the following algorithms for *Elastic Mixed-Criticality* model:

*  Scheduling with local early release
*  Scheduling with global early release
*  Scheduling with multiprocessor slack reclamation


--------
###  Overview ###

The core traits of the package are *Job*, *Task*, and *Scheduler*. 

**Job** has release time, deadline and length. It must be executed in the time window from *release time* to *deadline* for *length* units of time. 

**Task** is an infinite generator of jobs. 

**Scheduler** decide which jobs are executes at each moment of time. 

Using these abstract classes, we define special multi critical tasks in two traits: *LoCriticalTask* and *HiCriticalTask*. 

The scheduling of jobs produces by multi critical tasks is done in three classes: *ScheduleWithGlobalER*, *ScheduleWithLocalER*, *SwapSchedule*.