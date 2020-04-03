FROM debian:stretch-slim
ADD bin/lbtest /sbin/lbtest
CMD ["lbtest"]
