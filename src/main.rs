use std::os::fd::AsFd;

use linux_media as media;

fn main() {
    let path = "/dev/media0";
    let info = media::MediaDeviceInfo::from_path(path);
    match &info {
        Ok((fd, info)) => println!("info: ({:?},{:?})", fd, info),
        Err(err) => println!("err: {}", err),
    }
    let (info_fd, info) = info.unwrap();
    let topology = media::MediaTopology::new(&info, path);
    match &topology {
        Ok((fd, topology)) => println!("topology: ({:?},{:?})", fd, topology),
        Err(err) => println!("err: {}", err),
    }
    let (_fd, topology) = topology.unwrap();

    let mut es = media::MediaEntityIter::new(
        info_fd.as_fd(),
        info.media_version,
        topology.entities()[0].id(),
    );
    for e in es {
        println!("entity: {:?}", e);
    }
}
