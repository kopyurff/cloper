            Compressing and Optimizing a Virtual Machine's Undoable Disks
                                   Cloper utility
                                 (Cloning optimizer)

                             Copyright (C) Anton Kopiev,
                             GNU General Public License.

INTRODUCTION

 The Cloper utility is a console program for optimizing rollback virtual disks in order to reduce the size of their file on the physical disk. The x86 version of the utility is intended for use on 32-bit and 64-bit OS; in the case of a 64-bit guest OS, the x64 version of the utility can be used. The utility was tested on VMWare Workstation v.8.0, but its operating scheme should be applicable to all variants of virtual machine players, if they do not initialize the entire allocated disk space when creating it, but only store and record actual data. If, during the initial initialization of a virtual disk, the machine player allocates for it as much space as is specified for the disk volume, then using this utility becomes meaningless.
 
 The utility is used on a virtual machine with Windows OS, to which the source disk and the target etalon disk are connected in rollback or undoable mode. The source and target disks for creating an optimized snapshot must be copies of the same virtual disk, and the target disk contains only the original etalon data without the added information in the source disk. In general, the optimization procedure consists of comparing the data of two disk copies with low-level processing of the source disk data so that the physical data of its files are located, if possible, opposite the same physical data on the target reference disk. After the comparison procedures are completed, the data of the source disk is cloned to the etalon disk. Since cloning only records data that differs from the contents of the target reference disk, its resulting file size on the physical disk is significantly reduced.

OPTIMIZATION PROCEDURES

 The optimization procedure can be applied to virtual disks with MBR and GPT partition layout formats. The mapping operation and other optimization actions are applied only to partitions with the NTFS file system, partitions with other file systems are cloned as is. The utility provides the following procedures for optimizing NTFS volumes:
1. Preliminary deletion of the source files specified for this purpose, exclusion from the procedures of identical files with the same cluster arrangement in the source and in the standard;
2. NTFS compression of new and changed data with their consolidation (i.e. if specified, it also moves this source data in front of etalon data without matches);
3. Finds source file cluster matches among unmatched target file clusters and moves these source file data in front of matched target clusters;
4. Transfer of non-zero sectors of the etalon to file-free areas of the source volume;
5. Because during cloning only non-zero sectors are transferred, the remaining "non-zero" free sectors of the source volume are zeroed out.

PREREQUISITES

 To be able to perform optimization task the guest service OS must enable access to target & source disks using Windows API functionality, the active OS user of running task must have administrative privileges. The tool tasks can be started using Windows OS versions starting with XP SP2 x86/x64. It's recommended to use older OS versions in order to minimize background modifications of disk data by system during optimization task. Regardless of the task specification the tool actively works with data of source disk during all subtasks. The host system must be able to support these activities. To have optimal performance it is recommended to keep source & target disk snapshots on different hard drives.
 
 Depending on guest performance, amount of source & target data, task specification, the entire procedure can take from 10-20 minutes up to several hours. The resulting optimized snapshot can be obtained only after completion of all specified procedures. To avoid some background initializations of target data, it is recommended to power off guest VM or unmount target disk immediately after completion of task. Depending on task parameterization the system requirements to guest VM can be minimum or can require significant amount of RAM. The minimum RAM amount for running guest OS is prerequisite for all tasks except the subtask, which searches the matches of source data inside target data. The latter subtask can require up to 2 GB of RAM only for task process, it also notably consumes CPU time during search activities.

  If further use of the original virtual disk is expected, it is recommended to perform optimization after saving a copy of it. The nature of actions with its data is conventional fragmentation and the probability of their damage is not higher than the probability of file damage when copying to another folder, but due to internal data movement, previously unused sectors of the virtual disk are initialized and as a result, the size of its physical file grows to the actual disk volume.

UTILITY PARAMETERS

 The Cloper utility is a console program with command line parameters and a configuration file. Calling it without parameters on the command line displays a list of them with a description. The following command line parameters are provided:
1. "/help" or  "/h" - displays help on using the utility;
2. "/getconfig:short" or "/gc:short" - call to create a configuration file with default parameters. The suffix ":short" is optional, if it is not specified, a configuration file will be created with comments for each parameter and its value;
3. "/getserials:<drive letter>" or "/gs:<drive letter>" - gets the serial identifier of the disk, which consists of substrings of the identifiers of all its volumes separated by the "-" symbol. In order to get the identifier of a disk, it must have at least one volume with a letter assigned to it. Since the optimization procedure is performed using two copies of the same disk, their volume identifiers are the same;
4. "/serials:<serials>" or "/s:<serials>" - starts task for two disks with specified serials identifier;
5. "/serials:<serials> /showdiskdirs|sdd:<folders filter>" or "/[s]:<serials> /[sdd]:<folders filter>" - serves to display the list of folders according to the specified filter. This special call serves for additional control of the adequacy of the specified folder selection filter to the expected result. The value of the object selection filter for their preliminary deletion is specified in the configuration file in its section "[VolumeCleanup]". Also, see the help "/h" with section numbers 7-9 for additional details;
6. "/serials:<serials> /showdiskfiles|sdd:<files filter>" or "/[s]:<serials> /[sdf]:<files filter>" - the call is similar to the previous one, but to check the selection of files;
7. "/[signdisk]:<new signature>" or "/[sd]:<new signature>" - call to interactively change the disk signature. If no new value is specified, then lists connected disks with their signatures.
 When creating a configuration file, its values are set to the optimal value, if a file with comments is created, it has line-by-line help. When you first run the utility with purpose of optimization tasks on a virtual machine, it is recommended to create a new configuration file. To run the utility from automation scripts, the "RunMode" parameter must be changed to "SILENT", and the "PauseAtEnd" parameter to "NOT".

UTILITY PROJECT FILES

 Additional files have been added to the Cloper utility to better meet the criteria for open source software, for its documentation, and, where necessary, to make it easier to work with the source code. List of files with their relative locations:
1. ".\docs\Cloper utility for compressing and optimizing virtual machine rollback disk.pdf" - description of the utility in English;
2. ".\docs\Утилита Cloper - сжатие и оптимизация диска отката виртуальной машины.pdf" - description of the utility in Russian;
3. ".\project\..." - the original project of the utility was in Pascal, debugging and compilation of the published version of the program was carried out using Embarcadero® RAD Studio XE5;
4. ".\cloper_x86.zip" - zip-archive of 32-bit version of the utility;
5. ".\cloper_x64.zip" - zip-archive of 64-bit version of the utility.

PROJECT STATUS, SUPPORT AND DEVELOPMENT

 The current set of utility functionality and the utility source code are operational and can be used on Windows OS from XP to version 11. This set of functionality is published here for general use as is. To solve other similar problems of processing the source data of virtual machines, the set of utility capabilities can be expanded or improved. For offers of financial support and for the development of the project, I am available at the following contacts:
 
                E-Mail:	kopyurff@yahoo.com, kopyurff@rambler.ru
                Mobile:	8-921-912-44-10
